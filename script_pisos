import re
import openpyxl
import datetime
import time
import sys
import os
import shelve
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
output_file.create_sheet(index = 0, title = today + '_p')
output_sheet = output_file[today + '_p']
output_sheet['A1'].value = 'Link'
output_sheet['B1'].value = 'RentPrice'
output_sheet['C1'].value = 'Surface'
output_sheet['D1'].value = 'Bedrooms'
output_sheet['E1'].value = 'Bathrooms'
output_sheet['F1'].value = 'Floor'
output_sheet['G1'].value = 'LastUpdate'
output_sheet['H1'].value = 'PropertySubtype'
output_sheet['I1'].value = 'Neighborhood'
output_sheet['J1'].value = 'Location'
output_sheet['K1'].value = 'Seller'
output_sheet['L1'].value = 'Description'
output_sheet['M1'].value = 'Latitude'
output_sheet['N1'].value = 'Longitude'
output_sheet['O1'].value = 'DistanceToCenter'

# Regular Expressions
update_date_pattern = re.compile(r'Actualizado el (\d\d/\d\d/\d\d\d\d)') # Defines pattern for last update date
price_pattern = re.compile(r'>((\d+.)?\d+) €') # Defines pattern for the rent price
surface_pattern = re.compile(r'(\d+) m') # Defines pattern for the surface
bedrooms_pattern = re.compile(r'(\d+) hab') # Defines pattern for num of bedrooms
bathrooms_pattern = re.compile(r'(\d+) bañ') # Defines pattern for num of bathrooms
floor_pattern = re.compile(r'(\d+)ª planta') # Defines pattern for the floor
neighborhood_pattern = re.compile(r'''ition">([\w\s\S,'`´./+-]+)</h2>''') # Defines pattern for the neighborhood
description_pattern = re.compile(r'''descriptionBody">([\w\s\S,'`´.<>/+-]*)</div>''') # Defines pattern for the text description
prop_subtype_pattern = re.compile(r'alquilar/(\w+)-') # Defines the pattern for the property subtype
delete_pattern = re.compile(r'''<h1 class="title">\w+ en alquiler en ''') # Defines chunk of text to be deleted

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
    max_page = input("How many pages of results do you want to analyse? ")
    max_page = int(max_page)

    browser = webdriver.Firefox()

    for i in range(0,max_page):
        # Sets the result page link
        if i != 0:
            link = 'https://www.pisos.com/alquiler/pisos-barcelona_capital/fecharecientedesde-desc/' + str(i+1) +'/'
        else:
            link = 'https://www.pisos.com/alquiler/pisos-barcelona_capital/fecharecientedesde-desc/' 

        browser.get(link)
        adpage_clean = soup(browser.page_source, "html.parser")
        results1 = adpage_clean.findAll('div', {'class': 'row clearfix'})
        results2 = adpage_clean.findAll('a', {'class': 'anuncioLink'})
        for j in results1:
            href = j.get('data-navigate-ref')
            if href != None:
                link_list.append('https://www.pisos.com/' + href)
        for j in results2:
            href = j.get('href')
            if href != None:
                link_list.append('https://www.pisos.com/' + href)

        print('\n' + str(i+1) + ' result page(s) checked, out of ' + str(max_page))

    backup_p = shelve.open('backup_' + today + '_p')
    backup_p['link_list'] = link_list
    backup_p.close()
    print('\nJust finished gathering the links. Starting with the individual ads now.')

else:
    backup_p = shelve.open('backup_' + today + '_p')
    link_list = backup_p['link_list']
    backup_p.close()

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
            print('\nError 403: Forbidden Access, probably. Might as well wait for a while (30s), then continue. Feel free to grab a coffee.')
            time.sleep(30)

    adpage_clean = soup(adpage_raw, "html.parser")

    try:
        price = adpage_clean.find('div', {'class': 'priceBox-price'})
        if price == None:
            price = adpage_clean.find('span', {'class': 're-DetailHeader-price'})
        price = price_pattern.search(str(price)).group(1).replace('.', '')
        update_date = adpage_clean.find('div', {'class': 'updated-date'})
        update_date = update_date_pattern.search(str(update_date)).group(1)
        features = str(adpage_clean.find('div', {'class': 'basicdata-info'}))
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
            prop_subtype = prop_subtype_pattern.search(link_list[i]).group(1)
        except:
            prop_subtype = None
        try:
            description = adpage_clean.find('div', {'class': 'description-container description-body'})
            description = description_pattern.search(str(description)).group(1).replace('<br/>', '\n')
        except:
            description = None
        try:
            neighborhood = adpage_clean.find('h2', {'class': 'position'})
            neighborhood = neighborhood_pattern.search(str(neighborhood)).group(1)
        except:
            neighborhood = None
        try:
            location = str(adpage_clean.find('h1', {'class': 'title'}))
            if delete_pattern.search(location) == None:
                location = location.replace('</h1>', '')
            else:
                location = location.replace(delete_pattern.search(location).group(0), '').replace('</h1>', '')
        except:
            location = None
        if adpage_clean.find('div', {'class': 'owner-data-logo'}) == None:
            seller = 'Particular'
        else:
            seller = 'Professional'

        if location == None:
            locationString = neighborhood + ' Barcelona'
        else:
            locationString = neighborhood + ' ' + location + ' Barcelona'

        geocode_result = gmaps.geocode(locationString)
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
                             output_sheet['I' + row_excel].value,output_sheet['J' + row_excel].value, \
                             output_sheet['K' + row_excel].value, output_sheet['L' + row_excel].value, \
                             output_sheet['M' + row_excel].value, output_sheet['N' + row_excel].value, \
                             output_sheet['O' + row_excel].value = \
                             link_list[i], price, surface, bedrooms, bathrooms, floor, update_date, \
                             prop_subtype, neighborhood, location, seller, description, latitude, \
                             longitude, dist
        print('\nJust checked ' + str(i+1) + ' ad(s), out of ' + str(total_num))
    except Exception as e:
        print(e)
        print('\n')
        failed += 1

backup_locations['locDict'] = backup_locations_dict
backup_locations.close()
output_file.save(output_file_path)

end = time.time()
print('\nMinutes elapsed: ' + str((end - start)/60))
print(str(failed) + ' ad scraping attempts failed. The ratio of failure/total is ' + str(failed/total_num))
input('\nType anything to terminate the program. ')
sys.exit()
