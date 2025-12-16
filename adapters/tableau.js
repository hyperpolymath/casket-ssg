// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Tableau adapter - Static site generator in Elixir
 * https://github.com/elixir-tools/tableau
 */

export const name = "Tableau";
export const language = "Elixir";
export const description = "Modern static site generator for Elixir with LiveView support";

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
    name: "tableau_new",
    description: "Create a new Tableau project",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Project name" },
        path: { type: "string", description: "Path for the project" },
      },
      required: ["name"],
    },
    execute: async ({ name, path }) => {
      const args = ["tableau.new", name];
      return await runCommand(args, path);
    },
  },
  {
    name: "tableau_build",
    description: "Build the Tableau site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["tableau.build"], path),
  },
  {
    name: "tableau_server",
    description: "Start Tableau development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["tableau.server"];
      if (port) args.push("--port", String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "tableau_deps",
    description: "Fetch dependencies",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["deps.get"], path),
  },
  {
    name: "tableau_gen_post",
    description: "Generate a new post",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        title: { type: "string", description: "Post title" },
      },
      required: ["title"],
    },
    execute: async ({ path, title }) => await runCommand(["tableau.gen.post", title], path),
  },
  {
    name: "tableau_version",
    description: "Get Tableau/Mix version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
