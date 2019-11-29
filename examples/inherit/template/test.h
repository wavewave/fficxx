#pragma once

class Impl {
public:
    Impl() {}
    ~Impl() {}

    virtual void action( );
};

class Loader {

private:
  Impl *fImpl;

public:
  Loader( Impl* m ): fImpl(m) {}

  void invoke() {
    fImpl->action();
  }

};

class ImplSub : public Impl {
public:
    ImplSub() {}
    ~ImplSub() {}

    virtual void action( );
};
