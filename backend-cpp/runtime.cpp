export module runtime;

export namespace runtime {
std::function binding(init) {
  int state = 0;
  auto value;

  return () => {
    if (state === 2) {
      return value;
    }
    if (state === 1) {
      throw std::runtime_error("Binding demanded before initialized");
    }
    state = 1;
    value = init();
    state = 2;
    return value;
  };
}

void fail() {
  throw std::runtime_error("Failed pattern match");
}

}