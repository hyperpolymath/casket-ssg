// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Zotonic adapter - CMS/Framework in Erlang
 * https://zotonic.com/
 */

export const name = "Zotonic";
export const language = "Erlang";
export const description = "Erlang web framework and CMS with static site export";

let connected = false;
let zotonicPath = "zotonic";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(zotonicPath, {
    args,
    cwd: cwd || Deno.cwd(),
    stdout: "piped",
    stderr: "piped",
  });
  const output = await cmd.output();
  const decoder = new TextDecoder();
  return {
    success: output.success,
    stdout: decoder.decode(output.stdout),
    stderr: decoder.decode(output.stderr),
    code: output.code,
  };
}

export async function connect() {
  try {
    const result = await runCommand(["--version"]);
    connected = result.success;
    return connected;
  } catch {
    connected = false;
    return false;
  }
}

export async function disconnect() {
  connected = false;
}

export function isConnected() {
  return connected;
}

export const tools = [
  {
    name: "zotonic_start",
    description: "Start Zotonic",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to Zotonic root" },
      },
    },
    execute: async ({ path }) => await runCommand(["start"], path),
  },
  {
    name: "zotonic_stop",
    description: "Stop Zotonic",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to Zotonic root" },
      },
    },
    execute: async ({ path }) => await runCommand(["stop"], path),
  },
  {
    name: "zotonic_addsite",
    description: "Add a new site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to Zotonic root" },
        name: { type: "string", description: "Site name" },
      },
      required: ["name"],
    },
    execute: async ({ path, name }) => await runCommand(["addsite", name], path),
  },
  {
    name: "zotonic_status",
    description: "Show Zotonic status",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to Zotonic root" },
      },
    },
    execute: async ({ path }) => await runCommand(["status"], path),
  },
  {
    name: "zotonic_shell",
    description: "Start Erlang shell",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to Zotonic root" },
      },
    },
    execute: async ({ path }) => await runCommand(["shell"], path),
  },
  {
    name: "zotonic_compile",
    description: "Compile Zotonic",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to Zotonic root" },
      },
    },
    execute: async ({ path }) => await runCommand(["compile"], path),
  },
  {
    name: "zotonic_version",
    description: "Get Zotonic version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
