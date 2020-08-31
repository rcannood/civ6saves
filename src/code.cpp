#include <Rcpp.h>
using namespace Rcpp;

#include <string>
#include <fstream>
#include <iterator>
#include <iostream>
#include <algorithm>

//' @useDynLib civ6saves
//' @export
// [[Rcpp::export]]
void readMap(std::string path) {

  std::ifstream ifs(path);

  ifs.seekg(0, std::ios::end);
  std::streamsize f_size = ifs.tellg();
  ifs.seekg(0, std::ios::beg);

  std::vector<char> buffer(f_size);
  ifs.read(buffer.data(), f_size);

  std::vector<char> seq = {0x0E, 0x00, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00};


  auto it = std::search(buffer.begin(), buffer.end(), seq.begin(), seq.end());
  bool found = it != buffer.end();

  if (found) {
    std::cout << "found" << std::endl;
  } else {
    std::cout << "not found" << std::endl;
  }
  /*
  std::ifstream ifs(path, std::ios::in | std::ios::binary);

  // begin = start of file
  // end = end of file
  std::istream_iterator<char> begin(ifs), end, it;

  // string to match
  //std::string str = "\x0E\x00\x00\x00\x0F\x00\x00\x00\x06\x00\x00\x00\x00";
  std::string str = "\x0E\x00\x00\x00\x0F\x00\x1F\xF1\x16\x00\x00\x00\x00\x00\x0F\x00\x1F\xF1\x16\x00\x00\x00\x00\x00\x0F\x00\x1F\xF1\x16\x00\x00\x00\x00\x00\x0F\x00\x1F\xF1\x16\x00\x00\x00\x00";

  // search file for string
  it = std::search(begin, end, str.begin(), str.end());
  if (it != end)
  {
    std::cout << "found" << std::endl;
    //int diff = it.pointer - begin.pointer;
    //std::cout << "found at position " << diff << std::endl;
  }

  ifs.close();

  //return NULL;
   */
}
