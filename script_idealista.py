import openpyxl
import os
import datetime
import subprocess
import json
import shelve
import requests
import math
import random

today = str(datetime.date.today())

os.chdir("your_working_directory")
backup_i = shelve.open('backup_' + today + '_i')
output_file_path = 'your_excel_filepath'
output_file = openpyxl.load_workbook(output_file_path)
output_file.create_sheet(index = 0, title = today + '_i')
output_sheet = output_file[today + '_i']
output_sheet['A1'].value = 'Link'
output_sheet['B1'].value = 'RentPrice'
output_sheet['C1'].value = 'Surface'
output_sheet['D1'].value = 'Bedrooms'
output_sheet['E1'].value = 'Bathrooms'
output_sheet['F1'].value = 'PropertySubtype'
output_sheet['G1'].value = 'Municipality'
output_sheet['H1'].value = 'Neighborhood'
output_sheet['I1'].value = 'Latitude'
output_sheet['J1'].value = 'Longitude'
output_sheet['K1'].value = 'DistanceToCenter'
output_sheet['L1'].value = 'Floor'

def test(dictionary, key):
	try:
	    return dictionary[key]
	except:
	    return None

def distance(lat1, lng1, lat2, lng2):
    if lat2 == None or lng2 == None:
        return None
    elif (lat1 == lat2) and (lng1 == lng2):
        return 0
    else:
        theta = lng1-lng2
        dist = math.sin(math.radians(lat1)) * math.sin(math.radians(lat2)) + math.cos(math.radians(lat1)) * math.cos(math.radians(lat2)) * math.cos(math.radians(theta))
        dist = math.acos(dist)
        dist = math.degrees(dist)
        kilometers = dist * 60 * 1.1515 * 1.609344;
        return kilometers

# Start of the functional code:
headers = {
    'Authorization': 'Basic your_idealista_api_key',
    'Content-Type': 'application/x-www-form-urlencoded',
}

data = {
  'grant_type': 'client_credentials',
  'scope': 'read'
}

response = requests.post('https://api.idealista.com/oauth/token', headers=headers, data=data, verify=False)
response = response.text.replace("'", '"')
response = json.loads(response)
access_token = response['access_token']

code = ('''curl -X POST -H "Authorization: Bearer '''
        '''%s" '''
        '''-H "Content-Type: multipart/form-data;" '''
        '''-F "center=41.386631,2.169380" '''
        '''-F "propertyType=homes" '''
        '''-F "distance=9000" '''
        '''-F "operation=rent" '''
        '''-F "maxItems=50" '''
        '''-F "numPage=1" '''
        '''-F "sinceDate=M" '''
        '''"https://api.idealista.com/3.5/es/search"''') % access_token

output = subprocess.run(code, stdout=subprocess.PIPE)
try:
    json_output = output.stdout.decode('utf-8').replace("'", '"')
    json_output = json.loads(json_output)
except:
    json_output = output.stdout.decode('utf-8')
    json_output = json.loads(json_output)

totalPages = json_output['totalPages']

backup_i["totalPages"] = totalPages
backup_i["Page 1"] = json_output


row_excel_int = 2
for j in json_output['elementList']:
    row_excel = str(row_excel_int)
    output_sheet['A' + row_excel].value, output_sheet['B' + row_excel].value, \
                    output_sheet['C' + row_excel].value, output_sheet['D' + row_excel].value, \
                    output_sheet['E' + row_excel].value, output_sheet['F' + row_excel].value, \
                    output_sheet['G' + row_excel].value, output_sheet['H' + row_excel].value, \
                    output_sheet['I' + row_excel].value, output_sheet['J' + row_excel].value, \
                    output_sheet['K' + row_excel].value, output_sheet['L' + row_excel].value = \
                    test(j,'url'), test(j,'price'), test(j,'size'), test(j,'rooms'), test(j,'bathrooms'), \
                    test(j,'propertyType'), test(j,'municipality'), test(j,'neighborhood'), \
                    test(j,'latitude'), test(j,'longitude'), \
                    str(distance(41.382542, 2.177100, j['latitude'], j['longitude'])), test(j,'floor')
    row_excel_int += 1
            

selection = random.sample(range(2, totalPages + 1), k = 99)
evaluated = [1]

for i in selection:
    code = ('''curl -X POST -H "Authorization: Bearer '''
        '''%s" '''
        '''-H "Content-Type: multipart/form-data;" '''
        '''-F "center=41.386631,2.169380" '''
        '''-F "propertyType=homes" '''
        '''-F "distance=9000" '''
        '''-F "operation=rent" '''
        '''-F "maxItems=50" '''
        '''-F "numPage=%d" '''
        '''-F "sinceDate=M" '''
        '''"https://api.idealista.com/3.5/es/search"''') % (access_token, i)

    try:
        output = subprocess.run(code, stdout=subprocess.PIPE)
        try:
            json_output = output.stdout.decode('utf-8').replace("'", '"')
            json_output = json.loads(json_output)
        except:
            json_output = output.stdout.decode('utf-8')
            json_output = json.loads(json_output)
    except Exception as e:
        print(e)
        break

    backup_i["Page %d" % i] = json_output
    
    for j in json_output['elementList']:
        row_excel = str(row_excel_int)
        output_sheet['A' + row_excel].value, output_sheet['B' + row_excel].value, \
                        output_sheet['C' + row_excel].value, output_sheet['D' + row_excel].value, \
                        output_sheet['E' + row_excel].value, output_sheet['F' + row_excel].value, \
                        output_sheet['G' + row_excel].value, output_sheet['H' + row_excel].value, \
                        output_sheet['I' + row_excel].value, output_sheet['J' + row_excel].value, \
                        output_sheet['K' + row_excel].value, output_sheet['L' + row_excel].value = \
                        test(j,'url'), test(j,'price'), test(j,'size'), test(j,'rooms'), test(j,'bathrooms'), \
                        test(j,'propertyType'), test(j,'municipality'), test(j,'neighborhood'), \
                        est(j,'latitude'), test(j,'longitude'), \
                        str(distance(41.382542, 2.177100, j['latitude'], j['longitude'])),  test(j,'floor')
        row_excel_int += 1
    evaluated.append(i)


backup_i['PagesToCheck'] = selection
backup_i['PagesEvaluated'] = evaluated
             
backup_i.close()
output_file.save(output_file_path)
