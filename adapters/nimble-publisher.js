// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * NimblePublisher adapter - Markdown-based publishing for Phoenix
 * https://github.com/dashbitco/nimble_publisher
 */

export const name = "NimblePublisher";
export const language = "Elixir";
export const description = "Markdown-based publishing engine for Phoenix/Elixir applications";

let connected = false;
let mixPath = "mix";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(mixPath, {
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
    name: "nimble_publisher_new",
    description: "Create a new Phoenix project with NimblePublisher",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Project name" },
        path: { type: "string", description: "Path for the project" },
      },
      required: ["name"],
    },
    execute: async ({ name, path }) => {
      const args = ["phx.new", name, "--no-ecto"];
      return await runCommand(args, path);
    },
  },
  {
    name: "nimble_publisher_deps",
    description: "Fetch and compile dependencies",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["deps.get"], path),
  },
  {
    name: "nimble_publisher_compile",
    description: "Compile the project",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["compile"], path),
  },
  {
    name: "nimble_publisher_server",
    description: "Start Phoenix server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["phx.server"], path),
  },
  {
    name: "nimble_publisher_build",
    description: "Build release",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["release"], path),
  },
  {
    name: "nimble_publisher_version",
    description: "Get Mix/Elixir version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
