// Gallery.Main FFI
export const getLocationHash = () => window.location.hash;

// Reload on hash change so routing re-evaluates
export const onHashChange = (cb) => () => {
  window.addEventListener("hashchange", () => cb());
};

export const reloadPage = (_unit) => () => {
  window.location.reload();
};
