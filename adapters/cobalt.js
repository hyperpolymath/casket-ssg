// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Cobalt adapter - Static site generator in Rust
 * https://cobalt-org.github.io/
 */

export const name = "Cobalt";
export const language = "Rust";
export const description = "Straightforward static site generator written in Rust";

let connected = false;
let binaryPath = "cobalt";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(binaryPath, {
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
    name: "cobalt_init",
    description: "Initialize a new Cobalt site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
      },
    },
    execute: async ({ path }) => {
      const args = ["init"];
      if (path) args.push(path);
      return await runCommand(args);
    },
  },
  {
    name: "cobalt_build",
    description: "Build the Cobalt site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        destination: { type: "string", description: "Output directory" },
        drafts: { type: "boolean", description: "Include drafts" },
      },
    },
    execute: async ({ path, destination, drafts }) => {
      const args = ["build"];
      if (destination) args.push("--destination", destination);
      if (drafts) args.push("--drafts");
      return await runCommand(args, path);
    },
  },
  {
    name: "cobalt_serve",
    description: "Start Cobalt development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
        host: { type: "string", description: "Host to bind to" },
        drafts: { type: "boolean", description: "Include drafts" },
      },
    },
    execute: async ({ path, port, host, drafts }) => {
      const args = ["serve"];
      if (port) args.push("--port", String(port));
      if (host) args.push("--host", host);
      if (drafts) args.push("--drafts");
      return await runCommand(args, path);
    },
  },
  {
    name: "cobalt_watch",
    description: "Watch for changes and rebuild",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["watch"], path),
  },
  {
    name: "cobalt_clean",
    description: "Clean the build directory",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["clean"], path),
  },
  {
    name: "cobalt_new",
    description: "Create a new post",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        title: { type: "string", description: "Post title" },
      },
      required: ["title"],
    },
    execute: async ({ path, title }) => await runCommand(["new", title], path),
  },
  {
    name: "cobalt_version",
    description: "Get Cobalt version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
