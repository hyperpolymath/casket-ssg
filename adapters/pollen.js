// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Pollen adapter - Publishing system in Racket
 * https://docs.racket-lang.org/pollen/
 */

export const name = "Pollen";
export const language = "Racket";
export const description = "Publishing system for books and long-form content in Racket";

let connected = false;
let racoPath = "raco";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(racoPath, {
    args: ["pollen", ...args],
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
    const cmd = new Deno.Command(racoPath, {
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

export async function disconnect() {
  connected = false;
}

export function isConnected() {
  return connected;
}

export const tools = [
  {
    name: "pollen_start",
    description: "Start Pollen project server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["start"];
      if (port) args.push(String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "pollen_render",
    description: "Render Pollen source files",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        file: { type: "string", description: "Specific file to render" },
      },
    },
    execute: async ({ path, file }) => {
      const args = ["render"];
      if (file) args.push(file);
      return await runCommand(args, path);
    },
  },
  {
    name: "pollen_publish",
    description: "Publish the project",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        dest: { type: "string", description: "Destination directory" },
      },
    },
    execute: async ({ path, dest }) => {
      const args = ["publish"];
      if (dest) args.push(dest);
      return await runCommand(args, path);
    },
  },
  {
    name: "pollen_reset",
    description: "Reset Pollen cache",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["reset"], path),
  },
  {
    name: "pollen_setup",
    description: "Run setup for Pollen sources",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["setup"], path),
  },
  {
    name: "pollen_version",
    description: "Get Pollen/Racket version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["version"]),
  },
];
