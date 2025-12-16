// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Fornax adapter - Static site generator in F#
 * https://github.com/ionide/Fornax
 */

export const name = "Fornax";
export const language = "F#";
export const description = "Scriptable static site generator using F# and type providers";

let connected = false;
let fornaxPath = "fornax";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(fornaxPath, {
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

async function runDotnet(args, cwd = null) {
  const cmd = new Deno.Command("dotnet", {
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
    const result = await runCommand(["version"]);
    connected = result.success;
    return connected;
  } catch {
    // Try dotnet tool
    try {
      const result = await runDotnet(["fornax", "version"]);
      connected = result.success;
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
    name: "fornax_new",
    description: "Create a new Fornax site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
      },
    },
    execute: async ({ path }) => await runCommand(["new"], path),
  },
  {
    name: "fornax_build",
    description: "Build the Fornax site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["build"], path),
  },
  {
    name: "fornax_watch",
    description: "Start Fornax watch server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["watch"];
      if (port) args.push("--port", String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "fornax_clean",
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
    name: "fornax_version",
    description: "Get Fornax version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["version"]),
  },
];
