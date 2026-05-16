/**
 * Pi MCP Extension
 *
 * Connects to Model Context Protocol (MCP) servers and exposes their
 * tools as native Pi tools. Supports both stdio-based local servers
 * and Streamable HTTP / SSE-based remote servers.
 *
 * Requirements: Adds `@modelcontextprotocol/sdk` as a dependency —
 * `npm install` is run automatically by home-manager activation.
 *
 * Configuration is read from ~/.config/pi/mcp.json by default or
 * from a path set via the PI_MCP_CONFIG env var.
 *
 * ## Config format (~/.config/pi/mcp.json)
 *
 * ```json
 * {
 *   "mcpServers": {
 *     "filesystem": {
 *       "command": "npx",
 *       "args": ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
 *       "env": { "NODE_ENV": "production" }
 *     },
 *     "remote-api": {
 *       "url": "https://mcp.example.com/sse"
 *     }
 *   }
 * }
 * ```
 *
 * ## Usage
 *
 * - MCP tools are registered as `mcp_<server>_<toolname>` in Pi.
 * - Use `/mcp` to list connected servers and their tools.
 * - Tools reconnect automatically on `/reload`.
 */

import type {
  ExtensionAPI,
  ExtensionContext,
  Theme,
  ToolRenderContext,
  ToolRenderResultOptions,
} from "@mariozechner/pi-coding-agent";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { StreamableHTTPClientTransport } from "@modelcontextprotocol/sdk/client/streamableHttp.js";
import type { Tool, ContentBlock, CallToolResult } from "@modelcontextprotocol/sdk/types.js";
import { Text } from "@mariozechner/pi-tui";
import { Type, type TSchema } from "typebox";
import { readFileSync, existsSync, mkdirSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, dirname } from "node:path";

// ─── Types ──────────────────────────────────────────────────────────────────

interface MCPServerConfigStdio {
  command: string;
  args?: string[];
  env?: Record<string, string>;
  cwd?: string;
}

interface MCPServerConfigHTTP {
  url: string;
  headers?: Record<string, string>;
}

type MCPServerConfig = MCPServerConfigStdio | MCPServerConfigHTTP;

interface MCPConfig {
  mcpServers: Record<string, MCPServerConfig>;
}

interface ServerConnection {
  name: string;
  client: Client;
  tools: Tool[];
}

// ─── Config Loading ─────────────────────────────────────────────────────────

const DEFAULT_CONFIG_PATH = join(homedir(), ".config", "pi", "mcp.json");

function loadConfig(): MCPConfig {
  const configPath = process.env.PI_MCP_CONFIG || DEFAULT_CONFIG_PATH;

  if (!existsSync(configPath)) {
    // Create empty config with a commented stub so users know the path
    mkdirSync(dirname(configPath), { recursive: true });
    writeFileSync(configPath, JSON.stringify({ mcpServers: {} }, null, 2) + "\n", "utf-8");
    return { mcpServers: {} };
  }

  try {
    return JSON.parse(readFileSync(configPath, "utf-8")) as MCPConfig;
  } catch (err) {
    console.error(`[pi-mcp] Failed to parse config at ${configPath}:`, err);
    return { mcpServers: {} };
  }
}

// ─── Connection Management ──────────────────────────────────────────────────

function isStdioConfig(config: MCPServerConfig): config is MCPServerConfigStdio {
  return "command" in config;
}

async function connectToServer(name: string, config: MCPServerConfig): Promise<Client> {
  const client = new Client(
    { name: "pi-mcp", version: "1.0.0" },
    { capabilities: {} },
  );

  if (isStdioConfig(config)) {
    const transport = new StdioClientTransport({
      command: config.command,
      args: config.args ?? [],
      env: config.env,
      cwd: config.cwd,
      stderr: "inherit",
    });
    await client.connect(transport);
  } else {
    const transport = new StreamableHTTPClientTransport(new URL(config.url), {
      headers: config.headers,
    });
    await client.connect(transport);
  }

  return client;
}

async function discoverTools(client: Client): Promise<Tool[]> {
  const allTools: Tool[] = [];
  let cursor: string | undefined;
  do {
    const result = await client.listTools({ cursor });
    allTools.push(...result.tools);
    cursor = result.nextCursor;
  } while (cursor);
  return allTools;
}

// ─── Schema Conversion ──────────────────────────────────────────────────────

/**
 * Convert an MCP tool's JSON Schema inputSchema to a TypeBox schema.
 * Handles the common patterns (string, number, integer, boolean, array, object);
 * defaults to Unknown for anything exotic.
 */
function mcpSchemaToTypeBox(inputSchema: Tool["inputSchema"]): TSchema {
  if (!inputSchema || typeof inputSchema !== "object" || !inputSchema.properties) {
    return Type.Object({});
  }

  const properties: Record<string, TSchema> = {};
  const required = new Set<string>(
    Array.isArray(inputSchema.required) ? inputSchema.required.map(String) : [],
  );

  for (const [key, raw] of Object.entries(inputSchema.properties)) {
    const prop = raw as Record<string, unknown>;
    let ts: TSchema;

    switch (prop.type) {
      case "string":
        ts = Type.String();
        break;
      case "number":
        ts = Type.Number();
        break;
      case "integer":
        ts = Type.Integer();
        break;
      case "boolean":
        ts = Type.Boolean();
        break;
      case "array":
        ts = Type.Array(Type.Unknown());
        break;
      case "object":
        ts = Type.Record(Type.String(), Type.Unknown());
        break;
      default:
        ts = Type.Unknown();
    }

    if (typeof prop.description === "string") {
      (ts as Record<string, unknown>).description = prop.description;
    }

    if (!required.has(key)) {
      ts = Type.Optional(ts);
    }

    properties[key] = ts;
  }

  return Type.Object(properties);
}

// ─── Result Formatting ──────────────────────────────────────────────────────

function formatToolResult(result: CallToolResult): string {
  if (!result.content || result.content.length === 0) {
    return "(no output)";
  }

  return result.content
    .map((part: ContentBlock) => {
      switch (part.type) {
        case "text":
          return part.text;
        case "resource": {
          const r = part.resource;
          if ("text" in r) return r.text as string;
          if ("blob" in r) return `[Binary: ${r.mimeType ?? "unknown"} (${r.blob.length} bytes)]`;
          return JSON.stringify(r);
        }
        case "image":
          return `[Image: ${part.mimeType ?? "unknown"} (${part.data.length} bytes)]`;
        case "audio":
          return `[Audio: ${part.mimeType ?? "unknown"} (${part.data.length} bytes)]`;
        default:
          return JSON.stringify(part);
      }
    })
    .join("\n");
}

// ─── Extension ──────────────────────────────────────────────────────────────

export default async function (pi: ExtensionAPI) {
  const connections: ServerConnection[] = [];
  const toolToServer = new Map<string, { serverIndex: number; tool: Tool; serverName: string }>();

  async function connectServers() {
    // Close existing connections first
    for (const conn of connections) {
      try {
        await conn.client.close();
      } catch {
        // best-effort
      }
    }
    connections.length = 0;
    toolToServer.clear();

    const config = loadConfig();
    const entries = Object.entries(config.mcpServers);
    if (entries.length === 0) return;

    for (const [name, serverConfig] of entries) {
      try {
        const client = await connectToServer(name, serverConfig);
        const tools = await discoverTools(client);

        const serverIndex = connections.length;
        connections.push({ name, client, tools });

        for (const tool of tools) {
          const toolName = `mcp_${name}_${tool.name}`;
          toolToServer.set(toolName, { serverIndex, tool, serverName: name });

          pi.registerTool({
            name: toolName,
            label: `${name}: ${tool.name}`,
            description: tool.description ?? `MCP tool from server "${name}"`,
            promptSnippet: `${name} MCP tools`,
            promptGuidelines: [
              `Use \`${toolName}\` to invoke the \`${tool.name}\` tool exposed by the MCP server "${name}".`,
            ],
            parameters: mcpSchemaToTypeBox(tool.inputSchema),

            async execute(
              _toolCallId: string,
              params: Record<string, unknown>,
              signal: AbortSignal | undefined,
              _onUpdate: unknown,
              _ctx: ExtensionContext,
            ) {
              const entry = toolToServer.get(toolName);
              if (!entry) {
                return {
                  content: [{ type: "text" as const, text: `Error: MCP server "${name}" is not connected.` }],
                  details: {},
                };
              }

              try {
                const result = await entry.client.callTool(
                  { name: tool.name, arguments: params },
                  {},
                  { signal },
                );

                return {
                  content: [{ type: "text" as const, text: formatToolResult(result) }],
                  details: { serverName: name, toolName: tool.name, isError: result.isError },
                };
              } catch (err) {
                const msg = err instanceof Error ? err.message : String(err);
                return {
                  content: [{ type: "text" as const, text: `MCP error from "${name}"/"${tool.name}": ${msg}` }],
                  isError: true,
                  details: { serverName: name, toolName: tool.name, error: msg },
                };
              }
            },

            renderCall(
              args: Record<string, unknown>,
              theme: Theme,
              _context: ToolRenderContext,
            ) {
              const label = theme.fg("toolTitle", theme.bold(`mcp/${name}/${tool.name}`));
              const parts = Object.entries(args)
                .map(([k, v]) => `${k}=${JSON.stringify(v)}`)
                .join(", ");
              return new Text(parts ? `${label} ${theme.fg("muted", parts)}` : label, 0, 0);
            },

            renderResult(
              result: { content?: Array<{ type: string; text: string }>; isError?: boolean },
              _options: ToolRenderResultOptions,
              theme: Theme,
              _context: ToolRenderContext,
            ) {
              if (result.isError) {
                return new Text(theme.fg("error", result.content?.[0]?.text ?? "(error)"), 0, 0);
              }

              const text = result.content?.[0]?.text ?? "(no output)";
              const lines = text.split("\n");
              const preview = lines.slice(0, 5);
              const more = lines.length - 5;

              let output = "";
              for (const line of preview) {
                output += `${theme.fg("muted", line)}\n`;
              }
              if (more > 0) {
                output += theme.fg("dim", `... ${more} more line(s)`);
              }

              return new Text(output.trimEnd(), 0, 0);
            },
          });
        }

        console.error(`[pi-mcp] Connected "${name}" — ${tools.length} tool(s)`);
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        console.error(`[pi-mcp] Failed to connect "${name}": ${msg}`);
      }
    }
  }

  // ── Lifecycle hooks ──────────────────────────────────────────────────────

  pi.on("session_start", async () => { await connectServers(); });
  pi.on("resources_discover", async () => { await connectServers(); });

  // ── /mcp command ─────────────────────────────────────────────────────────

  pi.registerCommand("mcp", {
    description: "List connected MCP servers and their tools",
    handler: async (_args, ctx) => {
      if (!ctx.hasUI) {
        ctx.ui.notify("/mcp requires interactive mode", "error");
        return;
      }

      if (connections.length === 0) {
        ctx.ui.notify("No MCP servers connected", "info");
        return;
      }

      const lines: string[] = [];
      for (const c of connections) {
        lines.push(`  ${c.name}`);
        for (const t of c.tools) {
          lines.push(`    └ mcp_${c.name}_${t.name}: ${t.description ?? "no description"}`);
        }
      }

      ctx.ui.notify(`MCP Servers:\n${lines.join("\n")}`, "info");
    },
  });
}
