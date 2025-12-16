// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Reggae adapter - Static site generator in D
 * https://github.com/atilaneves/reggae
 */

export const name = "Reggae";
export const language = "D";
export const description = "Build system and static site generator in D";

let connected = false;
let reggaePath = "reggae";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(reggaePath, {
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
    name: "reggae_init",
    description: "Initialize Reggae build",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        backend: { type: "string", description: "Build backend (make, ninja, etc.)" },
      },
    },
    execute: async ({ path, backend }) => {
      const args = [];
      if (backend) args.push(`--backend=${backend}`);
      return await runCommand(args, path);
    },
  },
  {
    name: "reggae_build",
    description: "Build the project",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        target: { type: "string", description: "Build target" },
      },
    },
    execute: async ({ path, target }) => {
      // After reggae generates build files, use the backend
      const cmd = new Deno.Command("make", {
        args: target ? [target] : [],
        cwd: path || Deno.cwd(),
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
    },
  },
  {
    name: "reggae_clean",
    description: "Clean build artifacts",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => {
      const cmd = new Deno.Command("make", {
        args: ["clean"],
        cwd: path || Deno.cwd(),
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
    },
  },
  {
    name: "reggae_version",
    description: "Get Reggae version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
