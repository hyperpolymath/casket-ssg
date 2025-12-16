// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Frog adapter - Static blog generator in Racket
 * https://github.com/greghendershott/frog
 */

export const name = "Frog";
export const language = "Racket";
export const description = "Static blog generator written in Racket";

let connected = false;
let racoPath = "raco";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(racoPath, {
    args: ["frog", ...args],
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
    const result = await runCommand(["--help"]);
    connected = result.success || result.stderr.includes("frog");
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
    name: "frog_init",
    description: "Initialize a new Frog project",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new blog" },
      },
    },
    execute: async ({ path }) => await runCommand(["--init"], path),
  },
  {
    name: "frog_build",
    description: "Build the Frog blog",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to blog root" },
      },
    },
    execute: async ({ path }) => await runCommand(["--build"], path),
  },
  {
    name: "frog_preview",
    description: "Build and preview the blog",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to blog root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["--preview"];
      if (port) args.push("--port", String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "frog_new_post",
    description: "Create a new blog post",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to blog root" },
        title: { type: "string", description: "Post title" },
      },
      required: ["title"],
    },
    execute: async ({ path, title }) => await runCommand(["--new", title], path),
  },
  {
    name: "frog_clean",
    description: "Clean generated files",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to blog root" },
      },
    },
    execute: async ({ path }) => await runCommand(["--clean"], path),
  },
  {
    name: "frog_watch",
    description: "Watch for changes and rebuild",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to blog root" },
      },
    },
    execute: async ({ path }) => await runCommand(["--watch"], path),
  },
  {
    name: "frog_version",
    description: "Get Frog version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
