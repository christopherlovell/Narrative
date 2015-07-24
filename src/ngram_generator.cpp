#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name=".ngram_generator")]]
std::vector<std::string> ngram_generator(std::string sentence, unsigned int n){
	std::istringstream iss(sentence);
	std::vector<std::string> tokens;
	std::vector<std::string> bigrams;
	std::string str;

	copy(std::istream_iterator<std::string>(iss),
		std::istream_iterator<std::string>(),
		back_inserter(tokens));

	for (unsigned int i = n; i <= tokens.size(); ++i){
		str = tokens[i - n];
		for (unsigned int j = i - n + 1; j < i; j++){ str = str + ' ' + tokens[j]; }
		bigrams.push_back(str);
	}

	return(bigrams);
}

// [[Rcpp::export(name=".tdm_creator")]]
std::map<std::string, int> tdm_creator(std::vector<std::string> ngrams){
  std::map<std::string, int> tdm;
	std::vector<int> matched;

	int count;

	for (unsigned int i = 0; i < ngrams.size(); i++){
		if (std::find(matched.begin(), matched.end(), i) != matched.end()){
			continue;
		}

		count = 1;

		for (unsigned int j = i+1; j < ngrams.size(); j++){
			if (ngrams[i] == ngrams[j]){
				count++;	// increment counter variable if you get a match
				matched.push_back(j);	// save vector of matches to prevent re-reading the same variables
			}
		}

		tdm.insert(std::pair<std::string, int>(ngrams[i], count));
	}

	return(tdm);
}

// [[Rcpp::export(name=".tdm_generator")]]
Rcpp::DataFrame tdm_generator(std::vector<std::string> documents, int n){
	std::map<std::string, int> matches;
  
  std::vector<std::string> ngrams;
	std::vector<std::string> terms;
	std::vector<int> values;
	std::vector<int> doc_number;

	for (unsigned int i = 0; i < documents.size(); i++){
		ngrams = ngram_generator(documents[i], n);
	  matches = tdm_creator(ngrams);
		
		for(std::map<std::string, int>::iterator it = matches.begin(); it != matches.end(); it++){
			terms.push_back(it->first);
			values.push_back(it->second);
		}

	  std::vector<int> temp_array(matches.size(),i);
		
    doc_number.insert(doc_number.end(),temp_array.begin(),temp_array.end());
	}

	return Rcpp::DataFrame::create(Rcpp::Named("term") = terms, Rcpp::Named("count") = values, Rcpp::Named("doc") = doc_number);
}

