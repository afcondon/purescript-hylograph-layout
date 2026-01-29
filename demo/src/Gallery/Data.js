// Gallery.Data FFI
// Flare JSON data accessors

export const parseFlareJson = (jsonString) => JSON.parse(jsonString);

export const getName = (node) => node.name || "";

export const getValue = (node) => node.value || 0;

export const getChildren_ = (node) => node.children || null;
