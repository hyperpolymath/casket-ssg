// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Orchid adapter - Static site generator for Kotlin/Java
 * https://orchid.run/
 */

export const name = "Orchid";
export const language = "Kotlin";
export const description = "Powerful documentation and static site generator for Kotlin/Java";

let connected = false;
let gradlePath = "./gradlew";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(gradlePath, {
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
    const cmd = new Deno.Command("java", {
      args: ["-version"],
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
    name: "orchid_init",
    description: "Initialize a new Orchid project",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
        type: { type: "string", description: "Project type (docs, blog, wiki)" },
      },
    },
    execute: async ({ path, type }) => {
      // Orchid uses Gradle for project setup
      const args = ["init"];
      if (type) args.push("--type", type);
      return await runCommand(args, path);
    },
  },
  {
    name: "orchid_build",
    description: "Build the Orchid site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["orchidBuild"], path),
  },
  {
    name: "orchid_serve",
    description: "Start Orchid development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["orchidServe"];
      if (port) args.push(`-PorchidPort=${port}`);
      return await runCommand(args, path);
    },
  },
  {
    name: "orchid_deploy",
    description: "Deploy the Orchid site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["orchidDeploy"], path),
  },
  {
    name: "orchid_run",
    description: "Run Orchid (build + serve)",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["orchidRun"], path),
  },
  {
    name: "orchid_version",
    description: "Get Java/Gradle version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => {
      const cmd = new Deno.Command("java", {
        args: ["-version"],
        stdout: "piped",
        stderr: "piped",
      });
      const output = await cmd.output();
      const decoder = new TextDecoder();
      return {
        success: output.success,
        stdout: decoder.decode(output.stdout),
        stderr: decoder.decode(output.stderr),
      };
    },
  },
];
