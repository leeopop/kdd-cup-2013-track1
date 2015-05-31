#Copyright [2013] [Dmitry Efimov, Lucas Silva, Ben Solecki ]

#Licensed under the Apache License, Version 2.0 (the "License");
#you may not use this file except in compliance with the License.
#You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software
#distributed under the License is distributed on an "AS IS" BASIS,
#WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#See the License for the specific language governing permissions and
#limitations under the License.

import csv
import textmining
import re
from Common import *

def create_keyword_table():
	with open(os.path.join(get_paths()['working_dir'], 'Paper.csv'), 'r', encoding="utf-8") as paper_file:
		paper = csv.reader(paper_file)
		paper_column = paper.__next__()

		tdm = textmining.TermDocumentMatrix()

		papers = []
		for x in paper:
			papers.append(x)
			tdm.add_doc(re.sub(r"[^A-Za-z0-9 _]"," ",' '.join([str(x[1]),str(x[5])]), flags=re.UNICODE))

		keywords = []
		cutoff = 30
		stopwords = textmining.stopwords
		stopwords.update(['key', 'words', 'keywords', 'keyword', 'word'])
		for (paper_i, tdm_i) in zip(papers, tdm.sparse):
			id_paper = paper_i[0]
			year = paper_i[2]
			id_conference = paper_i[3]
			id_journal = paper_i[4]
			paper_words = [[id_paper,year,id_conference,id_journal,word] for word in tdm_i.keys() if tdm.doc_count[word] >= cutoff and word not in stopwords]
			keywords.extend(paper_words)

		with open(os.path.join(get_paths()['working_dir'],'keywords.csv'), 'w', encoding="utf-8") as keyword_file:
			keyword = csv.writer(keyword_file)
			keyword.writerow(('paperid', 'year', 'conferenceid', 'journalid', 'keyword'))
			for x in keywords:
				keyword.writerow(x)

if __name__ == "__main__":
	create_keyword_table()
	pass
