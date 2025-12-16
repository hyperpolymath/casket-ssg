// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Serum adapter - Static site generator in Elixir
 * https://dalgona.github.io/Serum/
 */

export const name = "Serum";
export const language = "Elixir";
export const description = "Simple static website generator written in Elixir";

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
    // Check if serum is available as a mix task
    const result = await runCommand(["help", "serum"]);
    connected = result.success || result.stderr.includes("serum");
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
    name: "serum_init",
    description: "Initialize a new Serum project",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
      },
      required: ["path"],
    },
    execute: async ({ path }) => await runCommand(["serum.new", path]),
  },
  {
    name: "serum_build",
    description: "Build the Serum site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        output: { type: "string", description: "Output directory" },
      },
    },
    execute: async ({ path, output }) => {
      const args = ["serum.build"];
      if (output) args.push("--output", output);
      return await runCommand(args, path);
    },
  },
  {
    name: "serum_server",
    description: "Start Serum development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number (default: 8080)" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["serum.server"];
      if (port) args.push("--port", String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "serum_gen_page",
    description: "Generate a new page",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        title: { type: "string", description: "Page title" },
        name: { type: "string", description: "Page filename" },
      },
      required: ["title"],
    },
    execute: async ({ path, title, name }) => {
      const args = ["serum.gen.page", title];
      if (name) args.push("--name", name);
      return await runCommand(args, path);
    },
  },
  {
    name: "serum_gen_post",
    description: "Generate a new blog post",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        title: { type: "string", description: "Post title" },
        tag: { type: "string", description: "Tags (comma-separated)" },
      },
      required: ["title"],
    },
    execute: async ({ path, title, tag }) => {
      const args = ["serum.gen.post", title];
      if (tag) args.push("--tag", tag);
      return await runCommand(args, path);
    },
  },
  {
    name: "serum_version",
    description: "Get Serum version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["serum", "--version"]),
  },
];
