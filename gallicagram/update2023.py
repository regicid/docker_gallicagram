import requests
from bs4 import BeautifulSoup
import os
page = requests.get(f"https://www.nytimes.com/search?dropmab=false&types=article&query=have&endDate=20231231&startDate=20230101")
soup = BeautifulSoup(page.content)
a = int(str(soup.find_all("script")[8]).split('totalCount":')[-1].split(",")[0])
os.system("head -n -1 base_presse_annees_nyt.csv > base_presse_annees_nyt.csv") ##Delete last line
os.system(f'echo 173,{a},2023' >> base_presse_annees_nyt.csv)