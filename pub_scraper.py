
##### Selenium try
import requests
import codecs
import time
import os
import pandas
from selenium import webdriver
from bs4 import BeautifulSoup
from urllib.parse import urljoin

# Open browser with chromedriver if you don't have it download it
chromrdriver = r"C:\Users\Proksenia\Documents\chromedriver.exe"
os.environ["webdriver.chrome.driver"] = chromrdriver
driver = webdriver.Chrome(executable_path=chromrdriver)

#Firstly we need to login in vk
driver.get('http://www.vk.com');
login = driver.find_element_by_css_selector('#index_email')
#input your login
login.send_keys('login')
password = driver.find_element_by_css_selector('#index_pass')
# input your pass
password.send_keys('password')
submit=driver.find_element_by_css_selector('#index_login_button')
submit.click()
time.sleep(3)

driver.get("https://vk.com/groups?act=catalog&c%5Bper_page%5D=40&c%5Bsection%5D=communities&c%5Bskip_catalog%5D=1&c%5Bsort%5D=6&c%5Btheme%5D=2126&c%5Btype%5D=4")
time.sleep(2)  # Allow 2 seconds for the web page to open
scroll_pause_time = 1  # You can set your own pause time. My laptop is a bit slow so I use 1 sec
document_height = 0  # set the initial scroll height

while document_height < 10000:
    # scroll to the end of the file
    driver.execute_script("window.scrollTo(0, 10000);")
    time.sleep(scroll_pause_time)
    # update scroll height each time after scrolled, as the scroll height can change after we scrolled the page
    document_height = driver.execute_script("return document.body.scrollHeight;")

#Creating HTML and inputing our data
file = open('DS.html', 'w', encoding="utf-8")
file.write(driver.page_source)
file.close()
driver.close()

# Search for the element we need
def scrape():
    test = [[],[]]
    with open('DS.html', 'r', encoding="utf-8") as data:
        soup = BeautifulSoup(data, 'html.parser')
        for links in soup.find_all('div', {'class': 'groups_row search_row clear_fix'}):
            link = links.get("data-id")
            test[0].append(link)
            for links in soup.find_all('img', {'class': 'search_item_img'}):
                link = links.get("alt")
                test[1].append(link)
        return(test)

vk_id = scrape()
# saving to csv
df = pandas.DataFrame(data={"grp_id": vk_id[0], "name": vk_id[1]})
df.to_csv("./groups.csv", sep=',',index=False)