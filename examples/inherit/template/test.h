#pragma once

#include <functional>

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

class ImplSub : public Impl {
private:
  std::function<void()>* fn;
public:
    ImplSub( void* fp ) {
      fn = static_cast<std::function<void()>*>(fp);
    }
    virtual ~ImplSub() {}

    virtual void action( );
};
