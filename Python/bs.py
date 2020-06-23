# beautiful soup试用
from urllib.request import urlopen 
from bs4 import BeautifulSoup

text = urlopen('https://www.crummy.com/software/BeautifulSoup/').read()

soup = BeautifulSoup(text, 'html.parser')
links = {}
for a in soup.find_all('a'):
    try:
        links[a.string] = a["href"]
    except KeyError: pass

print(links)

# {'Download': '#Download', 'Documentation': 'bs4/doc/', 'Hall of Fame': '#HallOfFame', 'For enterprise': 'enterprise.html', 'Source': 'https://code.launchpad.net/beautifulsoup',...}