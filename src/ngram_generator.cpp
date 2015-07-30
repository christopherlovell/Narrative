#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export(name=".ngram_generator")]]
std::vector<std::string> ngram_generator(std::string sentence, unsigned int n){
	std::istringstream iss(sentence);
	std::vector<std::string> tokens;
	std::vector<std::string> ngrams;
	std::string str;

	copy(std::istream_iterator<std::string>(iss),
		std::istream_iterator<std::string>(),
		back_inserter(tokens));

	for (unsigned int i = n; i <= tokens.size(); ++i){
		str = tokens[i - n];
		for (unsigned int j = i - n + 1; j < i; j++){ str = str + ' ' + tokens[j]; }
		ngrams.push_back(str);
	}
	
	std::sort(ngrams.begin(), ngrams.end());

	return(ngrams);
}


// [[Rcpp::export(name=".tdm_creator")]]
std::map<std::string, int> tdm_creator(std::vector<std::string> ngrams){
  std::map<std::string, int> tdm;
	std::vector<int> matched;

	int count = 1;

	for (unsigned int i = 0; i < ngrams.size() - 1; i++){
	  
	  if(ngrams[i] == ngrams[i+1]){
	    count++;
	    continue;
	  }else{
	    tdm.insert(std::pair<std::string, int>(ngrams[i], count));
	    count = 1;
	  }
	  
	  
	  /*
	  if(matched.size() == ngrams.size() - i){  // if all the remaining words have already been matched, break
	    break;
	  }
	  
		if (std::find(matched.begin(), matched.end(), i) != matched.end()){  // check element hasn't already been found
			continue;
		}

		count = 1;  // initialise count
		
		for (unsigned int j = i+1; j < ngrams.size(); j++){
			if (ngrams[i] == ngrams[j]){
				count++;	// increment counter variable if you get a match
				matched.push_back(j);	// save vector of matches to prevent re-reading the same variables
			}
		}

		tdm.insert(std::pair<std::string, int>(ngrams[i], count));
	   */
	}
	
	return(tdm);
}


// [[Rcpp::export(name="tdm_generator")]]
Rcpp::List tdm_generator(std::vector<std::string> documents, int n){
  std::vector<std::map<std::string, int> > matches;
  std::vector<std::string> ngrams;
	std::vector<std::string> terms;

	for (unsigned int i = 0; i < documents.size(); i++){
	  ngrams = ngram_generator(documents[i], n);
	  matches.push_back(tdm_creator(ngrams));
	  
	  //for (auto elem : matches[i]){
	  for(std::map<std::string, int>::iterator it = matches[i].begin(); it != matches[i].end(); it++) {
	    terms.push_back(it->first);  // create vector of all terms
	    //terms.push_back(elem.first);
	  }
	}
	
	std::vector<std::string> terms_unique = terms;
	
	// find all unique terms
	std::sort(terms_unique.begin(), terms_unique.end());
	terms_unique.erase(std::unique(terms_unique.begin(), terms_unique.end()), terms_unique.end());

	const int t = terms_unique.size();
	const int d = documents.size();

	arma::sp_mat dtm(d,t);
	int position;
	
	for (int i = 0; i < d; i++) {
	  
	  /*
	  
	  std::vector<std::string>::iterator beginner = terms_unique.begin();
	  
	  //for(auto elem : matches[i]){
	  for(std::map<std::string, int>::iterator it = matches[i].begin(); it != matches[i].end(); it++){
	    position = std::find(beginner, terms_unique.end(), it->first) - terms_unique.begin();
	    //dtm(i, position) = elem.second;
	    dtm(i,position) = it->second;
	    beginner++;
	  }
	  */
	  
	  std::map<std::string, int>::iterator it = matches[i].begin();
	  for (int j = 0; j < t; j++){
	    if (it->first == terms_unique[j]){
	      dtm(i, j) = it->second;
	      ++it;
	      if (it == matches[i].end()){
	        break;
	      }
	    }
	  }
	}
  
	return Rcpp::List::create(Rcpp::Named("terms") = terms_unique, Rcpp::Named("dtm") = dtm);
}

// std::vector<std::string>::iterator start = terms.begin();
// std::vector<std::string>::iterator end = terms.begin() + terms.size() / 2;

int RecursiveTreeSearch(std::vector<std::string> terms, std::string search, std::vector<std::string>::iterator start, std::vector<std::string>::iterator end){
  int range = end - start;
  
  if(range > 0){
    if(std::binary_search(start,end,search)){
      
      TreeSearch()
      
    }
  }else{
    return
  }
}


