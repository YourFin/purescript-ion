// Relies on the fact that all valid IonTypes are defined in the main file, and reference
// equality/pointer comparison is good enough to tell them apart
export function _ionTypeEqual(a, b) {
  return a === b
}
