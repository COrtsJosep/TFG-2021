import re
import openpyxl
import datetime
import time
import sys
import os
import shelve
import unidecode
import googlemaps
import math
from bs4 import BeautifulSoup as soup
from urllib.request import urlopen as uReq
from urllib.request import Request
from selenium import webdriver

# Definition of the output excel file
today = str(datetime.date.today())
os.chdir("your_working_directory")
output_file_path = 'your_excel_filepath'
output_file = openpyxl.load_workbook(output_file_path)
output_file.create_sheet(index = 0, title = today + '_f')
output_sheet = output_file[today + '_f']
output_sheet['A1'].value = 'Link'
output_sheet['B1'].value = 'RentPrice'
output_sheet['C1'].value = 'Surface'
output_sheet['D1'].value = 'Bedrooms'
output_sheet['E1'].value = 'Bathrooms'
output_sheet['F1'].value = 'LastUpdate'
output_sheet['G1'].value = 'PropertySubtype'
output_sheet['H1'].value = 'Location'
output_sheet['I1'].value = 'Seller'
output_sheet['J1'].value = 'Description'
output_sheet['K1'].value = 'Latitude'
output_sheet['L1'].value = 'Longitude'
output_sheet['M1'].value = 'DistanceToCenter'

# Regular Expressions
link_pattern = re.compile(r'href="((\S)*)"') # Defines the pattern for the link of each result
age_pattern = re.compile(r'hace (\d+) día') # Defines pattern for the age of the ad
location_pattern = re.compile(r'''</span>([\w\d\-\s.,\(\)'/`´]+)</h3>''') # Defines the pattern for the approximate location 
price_pattern = re.compile(r'>((\d+.)?\d+) €') # Defines pattern for the rent
surface_pattern = re.compile(r'(\d+)</span> m') # Defines pattern for the surface
bedrooms_pattern = re.compile(r'(\d+)</span> hab') # Defines pattern for num of bedrooms
bathrooms_pattern = re.compile(r'(\d+)</span> baño') # Defines pattern for num of bathrooms
prop_subtype_pattern = re.compile(r'>([\w\W]*)<') # Defines pattern for the subtypology of accommodation
floor_pattern = re.compile(r'(\d+)ª</span> Planta') # Defines pattern for the floor
description_pattern = re.compile(r'''Description">([\w\s\S,'`´.<>/+-]*)</p>''') # Defines pattern for the text description
location_pattern = re.compile(r'alquiler en ([\w\W]*)<') # Defines pattern to be deleted in the location text

# Geocoding preparation
maps_apikey = 'your_google_api_key'
gmaps = googlemaps.Client(key = maps_apikey)
def distance(lat1, lng1, lat2, lng2):
    if (lat1 == lat2) and (lng1 == lng2):
        return 0
    else:
        theta = lng1-lng2
        dist = math.sin(math.radians(lat1)) * math.sin(math.radians(lat2)) + math.cos(math.radians(lat1)) * math.cos(math.radians(lat2)) * math.cos(math.radians(theta))
        dist = math.acos(dist)
        dist = math.degrees(dist)
        kilometers = dist * 60 * 1.1515 * 1.609344;
        return kilometers
    
backup_locations = shelve.open('backup_locations')
backup_locations_dict = backup_locations['locDict']

# Start of the functional code
choice = input('Do you want to gather ad links anew or use the backup (anew/backup)? ')
while choice != 'anew' and choice != 'backup':
    choice = input('Do you want to gather ad links anew or use the backup (anew/backup)? ')

start = time.time()

if choice == 'anew':
    link_list = []
    time_ago_list = []
    sleep_time = 0.2
    max_page = input("How many pages of results do you want to analyse? ") # Attention: Do not surpass the maximum number of actual result pages. Check: https://www.fotocasa.es/es/alquiler/viviendas/barcelona-capital/todas-las-zonas/l?combinedLocationIds=724%2C9%2C8%2C232%2C376%2C8019%2C0%2C0%2C0&gridType=3&latitude=41.3854&longitude=2.17754&sortType=publicationDate
    max_page = int(max_page)

    browser = webdriver.Firefox()

    for i in range(0,max_page):
        # Sets the result page link
        if i != 0:
            link = "https://www.fotocasa.es/es/alquiler/viviendas/barcelona-capital/todas-las-zonas/l/" + str(i) + "?combinedLocationIds=724%2C9%2C8%2C232%2C376%2C8019%2C0%2C0%2C0&gridType=3&latitude=41.3854&longitude=2.17754&sortType=publicationDate"
        else:
            link = "https://www.fotocasa.es/es/alquiler/viviendas/barcelona-capital/todas-las-zonas/l?combinedLocationIds=724%2C9%2C8%2C232%2C376%2C8019%2C0%2C0%2C0&gridType=3&latitude=41.3854&longitude=2.17754&sortType=publicationDate"
        # The browser opens the page and scrolls through it in a stepwise manner
        browser.get(link)
        time.sleep(1)
        browser.execute_script("window.scrollTo(0, 1000)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 1500)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 2000)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 2500)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 3000)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 3500)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 4000)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 4500)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 5000)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 5500)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 6000)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 6500)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 7000)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 7500)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 8000)")
        time.sleep(sleep_time)
        browser.execute_script("window.scrollTo(0, 8500)")
        time.sleep(sleep_time)

        resultpage_raw = browser.page_source
        resultpage_clean = soup(resultpage_raw, "html.parser")
        results = resultpage_clean.findAll("div", {"class": "re-Card-primary"})
        found = 0
        for j in results:
            link_list.append('https://www.fotocasa.es' + link_pattern.search(str(j)).group(1))
            found += 1

        time_ago = resultpage_clean.findAll('span', {'class': 're-Card-timeago'})
        for j in time_ago:
            age = age_pattern.search(str(j))
            if age == None:
                time_ago_list.append(0)
            else:
                time_ago_list.append(age.group(1))
            
        print('\n\n' + str(i+1) + ' result page(s) checked, out of ' + str(max_page))
        print('\n' + str(found) + ' links were found in the result page.')
        
    backup_f = shelve.open('backup_' + today + '_f')
    backup_f['link_list'] = link_list
    backup_f['time_ago_list'] = time_ago_list
    backup_f.close()
    print('\nJust finished gathering the links. Starting with the individual ads now, gathered ' + str(len(link_list)/max_page) + ' links per result page.')

else:
    backup_f = shelve.open('backup_' + today + '_f')
    link_list = backup_f['link_list']
    time_ago_list = backup_f['time_ago_list']
    backup_f.close()

total_num = len(link_list)
failed = 0
hdrs = {'User-Agent': 'Mozilla / 5.0 (X11 Linux x86_64) AppleWebKit / 537.36 (KHTML, like Gecko) Chrome / 52.0.2743.116 Safari / 537.36'}
for i in range(0,total_num):
    while True:
        try:
            my_Req = Request(link_list[i], headers = hdrs)
            uClient = uReq(my_Req, timeout=50)
            adpage_raw = uClient.read()
            uClient.close()
            break
        except:
            print('\nError 403: Forbidden Access, probably. Waiting for a while (30s), then continuing.')
            time.sleep(30)
            
    
    adpage_clean = soup(adpage_raw, "html.parser")

    try:
        price = adpage_clean.findAll('span', {'class': 're-DetailHeader-price'})
        price = price_pattern.search(str(price)).group(1)

        features = str(adpage_clean.findAll('li', {'class': 're-DetailHeader-featuresItem'}))
        try:
            bedrooms = bedrooms_pattern.search(features).group(1)
        except:
            bedrooms = None
        try:
            bathrooms = bathrooms_pattern.search(features).group(1)
        except:
            bathrooms = None
        try:
            surface = surface_pattern.search(features).group(1)
        except:
            surface = None
        try:
            floor = floor_pattern.search(features).group(1)
        except:
            floor = None
        try:
            prop_subtype = str(adpage_clean.find('p', {'class': 're-DetailFeaturesList-featureValue'}))
            prop_subtype = prop_subtype_pattern.search(prop_subtype).group(1)
        except:
            prop_subtype = None
        try:
            description = adpage_clean.findAll('p', {'class': 'fc-DetailDescription'})
            description = description_pattern.search(str(description)).group(1).replace('<br/>', '\n').replace('<strong>', '').replace('</strong>', '')
        except:
            description = None
        try:
            location = str(adpage_clean.find('h1', {'class': 're-DetailHeader-propertyTitle'}))
            location = location_pattern.search(location).group(1)
        except:
            location = None
        if adpage_clean.find('img', {'class': 're-ContactDetail-inmoLogo'}) == None:
            seller = 'Particular'
        else:
            seller = 'Professional'

        locationString = location + ' Barcelona'
        if locationString in backup_locations_dict.keys():
            latitude = backup_locations_dict[locationString]['lat']
            longitude = backup_locations_dict[locationString]['lng']
            dist = backup_locations_dict[locationString]['dist']
        else:
            geocode_result = gmaps.geocode(locationString)
            latitude = geocode_result[0]['geometry']['location']['lat']
            longitude = geocode_result[0]['geometry']['location']['lng']
            dist = str(distance(41.382542, 2.177100, latitude, longitude))
            latitude, longitude = str(latitude), str(longitude)

            backup_locations_dict[locationString] = {'lat': latitude,
                                                     'lng': longitude,
                                                     'dist': dist}
                
        row_excel = str(i + 2 - failed)
        output_sheet['A' + row_excel].value, output_sheet['B' + row_excel].value, \
                             output_sheet['C' + row_excel].value, output_sheet['D' + row_excel].value, \
                             output_sheet['E' + row_excel].value, output_sheet['F' + row_excel].value, \
                             output_sheet['G' + row_excel].value, output_sheet['H' + row_excel].value, \
                             output_sheet['I' + row_excel].value, output_sheet['J' + row_excel].value, \
                             output_sheet['K' + row_excel].value, output_sheet['L' + row_excel].value, \
                             output_sheet['M' + row_excel].value = \
                             link_list[i], price, surface, bedrooms, bathrooms, time_ago_list[i], prop_subtype, \
                             location, seller, description, latitude, longitude, dist
        if (i+1) % 10 == 0:
            print('\nJust checked ' + str(i+1) + ' ad(s), out of ' + str(total_num))
            
    except Exception as e:
        print('\nScraping failed, link: ' + link_list[i])
        failed += 1

backup_locations['locDict'] = backup_locations_dict
backup_locations.close()
output_file.save(output_file_path)

end = time.time()
print('\nMinutes elapsed: ' + str((end - start)/60))
print(str(failed) + ' ad scraping attempts failed. The ratio of failure/total is ' + str(failed/total_num))
input('\nType anything to terminate the program. ')
sys.exit()
