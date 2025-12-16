// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Wub adapter - Web framework in Tcl
 * https://wiki.tcl-lang.org/page/Wub
 */

export const name = "Wub";
export const language = "Tcl";
export const description = "Web framework in Tcl with static site generation capabilities";

let connected = false;
let tclshPath = "tclsh";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(tclshPath, {
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
    const cmd = new Deno.Command(tclshPath, {
      args: ["<<", "puts [info patchlevel]"],
      stdout: "piped",
      stderr: "piped",
    });
    const output = await cmd.output();
    connected = output.success;
    return connected;
  } catch {
    // Try alternative
    try {
      const result = await runCommand(["--version"]);
      connected = true;
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
    name: "wub_start",
    description: "Start Wub server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to Wub root" },
        config: { type: "string", description: "Config file" },
      },
    },
    execute: async ({ path, config }) => {
      const args = [config || "wub.tcl"];
      return await runCommand(args, path);
    },
  },
  {
    name: "wub_generate",
    description: "Generate static files",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to Wub root" },
        output: { type: "string", description: "Output directory" },
      },
    },
    execute: async ({ path, output }) => {
      const script = `
        source wub.tcl
        package require Wub
        Wub generate ${output || "public"}
      `;
      return await runCommand(["-c", script], path);
    },
  },
  {
    name: "wub_run",
    description: "Run a Tcl script",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Working directory" },
        script: { type: "string", description: "Script file to run" },
      },
      required: ["script"],
    },
    execute: async ({ path, script }) => await runCommand([script], path),
  },
  {
    name: "wub_version",
    description: "Get Tcl version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => {
      const script = 'puts "Tcl [info patchlevel]"';
      return await runCommand(["-c", script]);
    },
  },
];
