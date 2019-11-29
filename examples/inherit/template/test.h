#pragma once

class Impl {
public:
    Impl() {}
    virtual ~Impl() {}

    virtual void action( );
};

class Loader {

private:
  Impl *fImpl;

public:
  Loader( Impl* m ): fImpl(m) {}
  virtual ~Loader() {}

  void invoke() {
    fImpl->action();
  }

};
