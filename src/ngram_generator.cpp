
#include <Rcpp.h>

//'@title ngram_generator
//'
//'@description C++ ngram generation for a vector of strings
//'
//'@param documents character vector to create ngrams for
//'@param n ngram size
//'@param delim delimiting character
// [[Rcpp::export(name=".ngram_generator")]]
std::vector<std::map<std::string, int> > ngram_generator(std::vector<std::string> documents, unsigned int n, char delim){
	std::vector<std::string> tokens;
	std::vector<std::string> ngrams;
	std::vector<std::map<std::string, int> > ngramFrequency(documents.size()); 
	std::string str;

	for(unsigned int k = 0; k < documents.size(); k++) {
	  std::transform(documents[k].begin(), documents[k].end(), documents[k].begin(), ::tolower);
	  
	  std::stringstream ss(documents[k]);
	  std::string item;
	  while (std::getline(ss, item, delim)) {
	    if (!item.empty())
	      tokens.push_back(item);
	  }
	  
	  for (unsigned int i = n; i <= tokens.size(); i++) {
	    str = tokens[i - n];
	    for (unsigned int j = i - n + 1; j < i; j++){ str = str + ' ' + tokens[j]; }
	    ngrams.push_back(str);
	  }
	  
	  tokens.clear();
	  
	  std::sort(ngrams.begin(), ngrams.end());
	 
	  std::vector<int> matched;
	  
	  int count = 1;
	  for (unsigned int i = 0; i <= ngrams.size() - 1; i++) {
	    if (i == ngrams.size() - 1){
	      ngramFrequency[k].insert(std::pair<std::string, int>(ngrams[i], count));
	      break;
	    }
	    if(ngrams[i] == ngrams[i+1]) {
	      count++;
	      continue;
	    }else{
	      ngramFrequency[k].insert(std::pair<std::string, int>(ngrams[i], count));
	      count = 1;
	    }
	  }
	  
	  ngrams.clear();
	}
	
	return(ngramFrequency);
}

