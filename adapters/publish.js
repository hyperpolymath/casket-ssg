// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Publish adapter - Static site generator in Swift
 * https://github.com/JohnSundell/Publish
 */

export const name = "Publish";
export const language = "Swift";
export const description = "Static site generator built for Swift developers by John Sundell";

let connected = false;
let publishPath = "publish";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(publishPath, {
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

async function runSwift(args, cwd = null) {
  const cmd = new Deno.Command("swift", {
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
    const result = await runCommand(["--help"]);
    connected = result.success;
    return connected;
  } catch {
    // Try swift run
    try {
      const result = await runSwift(["--version"]);
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
    name: "publish_new",
    description: "Create a new Publish website",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
      },
    },
    execute: async ({ path }) => await runCommand(["new"], path),
  },
  {
    name: "publish_generate",
    description: "Generate the website",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["generate"], path),
  },
  {
    name: "publish_run",
    description: "Start local development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["run"];
      if (port) args.push("--port", String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "publish_deploy",
    description: "Deploy the website",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["deploy"], path),
  },
  {
    name: "publish_build",
    description: "Build the Swift package",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runSwift(["build"], path),
  },
  {
    name: "publish_version",
    description: "Get Swift version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runSwift(["--version"]),
  },
];
