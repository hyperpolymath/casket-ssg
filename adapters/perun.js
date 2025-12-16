// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Perun adapter - Static site generator using Boot (Clojure)
 * https://perun.io/
 */

export const name = "Perun";
export const language = "Clojure";
export const description = "Composable static site generator using Boot build tool";

let connected = false;
let bootPath = "boot";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(bootPath, {
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
    name: "perun_build",
    description: "Build the Perun site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        task: { type: "string", description: "Boot task to run (default: build)" },
      },
    },
    execute: async ({ path, task }) => await runCommand([task || "build"], path),
  },
  {
    name: "perun_dev",
    description: "Start development server with watch",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["dev"], path),
  },
  {
    name: "perun_watch",
    description: "Watch and rebuild on changes",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["watch", "build"], path),
  },
  {
    name: "perun_serve",
    description: "Serve the built site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["serve"];
      if (port) args.push("-p", String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "perun_version",
    description: "Get Boot version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
