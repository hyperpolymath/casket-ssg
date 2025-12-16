// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Marmot adapter - Static site generator in Crystal
 * https://github.com/erdnaxeli/marmot
 */

export const name = "Marmot";
export const language = "Crystal";
export const description = "Static site generator written in Crystal";

let connected = false;
let marmotPath = "marmot";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(marmotPath, {
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

async function runShards(args, cwd = null) {
  const cmd = new Deno.Command("shards", {
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
    // Try crystal
    try {
      const cmd = new Deno.Command("crystal", {
        args: ["--version"],
        stdout: "piped",
        stderr: "piped",
      });
      const output = await cmd.output();
      connected = output.success;
      return connected;
    } catch {
      connected = false;
      return false;
    }
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
    name: "marmot_init",
    description: "Initialize a new Marmot site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
      },
    },
    execute: async ({ path }) => await runCommand(["init"], path),
  },
  {
    name: "marmot_build",
    description: "Build the Marmot site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        output: { type: "string", description: "Output directory" },
      },
    },
    execute: async ({ path, output }) => {
      const args = ["build"];
      if (output) args.push("--output", output);
      return await runCommand(args, path);
    },
  },
  {
    name: "marmot_serve",
    description: "Start development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["serve"];
      if (port) args.push("--port", String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "marmot_watch",
    description: "Watch and rebuild on changes",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["watch"], path),
  },
  {
    name: "marmot_clean",
    description: "Clean build output",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["clean"], path),
  },
  {
    name: "marmot_deps",
    description: "Install Crystal dependencies",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runShards(["install"], path),
  },
  {
    name: "marmot_version",
    description: "Get Marmot version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
