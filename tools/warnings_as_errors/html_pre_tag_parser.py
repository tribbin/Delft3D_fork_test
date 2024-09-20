import sys
from pathlib import Path
from html.parser import HTMLParser
from typing import Optional

class PreTagParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.in_body = False
        self.in_pre = False
        self.pre_tags = []

    def handle_starttag(self, tag, attrs):
        if tag == 'body':
            self.in_body = True
        if self.in_body and tag == 'pre':
            self.in_pre = True
            self.pre_tags.append('')

    def handle_endtag(self, tag):
        if tag == 'body':
            self.in_body = False
        if self.in_body and tag == 'pre':
            self.in_pre = False

    def handle_data(self, data):
        if self.in_pre:
            self.pre_tags[-1] += data

def extract_text_from_last_pre_tag(html_file_path: str) -> Optional[str]:
    with open(html_file_path, 'r', encoding='utf-16') as file:
        parser = PreTagParser()
        parser.feed(file.read())
        
        if not parser.pre_tags:
            return None
        
        # Get the text of the last <pre> tag
        return parser.pre_tags[-1]
