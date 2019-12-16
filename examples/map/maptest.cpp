#include <iostream>
#include <map>
#include <utility>

int main( int argc, char** argv ) {

  std::map<int, double> mymap ;

  mymap.insert (std::pair <int, double> (1,2.3)) ;
  mymap.insert (std::pair <int, double> (4,5.6));

  std::cout << "mymap.size() = " << mymap.size() << std::endl;
  std::cout << "mymap.at(1)  = " << mymap.at(1) << std::endl;

  auto it = mymap.find(2);
  if (it == mymap.end() ) {
    std::cout << "not found element at key = 2" << std::endl;
  }

  auto it2 = mymap.find(4);
  if (it2 == mymap.end() ) {
    std::cout << "not found element at key = 4" << std::endl;
  }
  else {
    std::cout << "it->second = " << it2->second << std::endl;
  }

  std::cout << "mymap.at(2)  = " << mymap.at(2) << std::endl;

  return 0;
}
