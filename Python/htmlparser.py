from html.parser import HTMLParser

class GrabH1(HTMLParser):
    h1 = False
    def handle_starttag(self, tag, attrs):
        if tag == "h1":
            self.h1 = True
    def handle_data(self, data):
        if self.h1 == True:
            print("DATA:", data)

    def handle_endtag(self, tag):
        if self.h1 and tag == "h1":
            self.h1 = False
text = open("./fixed.html").read()
parser = GrabH1()
parser.feed(text)
parser.close()

# DATA: Pet Shop
# DATA: Dead Pets