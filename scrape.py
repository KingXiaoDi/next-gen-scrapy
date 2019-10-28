"""
Author: Sarah Mallepalle

Use BeautifulSoup to scrape https://nextgenstats.nfl.com/ for all available extra-large (1200 x 1200)
pass chart images and corresponding pass chart data from the 2017 & 2018 NFL seasons, 
and save the image and its corresponding data to the folder 'Pass_Charts'.

Folder format: ./Pass_Charts/[team]/[season]/[week]/{[images], [data]}/[last_name]_[first_name]_[positon].{jpeg, txt}
Example: 
	Image path = ./Pass_Charts/philadelphia-eagles/2017/super-bowl/images/Foles_Nick_QB.jpeg
	Data path = ./Pass_Charts/philadelphia-eagles/2017/super-bowl/data/Foles_Nick_QB.txt
"""

from bs4 import BeautifulSoup 
import requests 
import urllib.request
import json
import sys
import re
import os

teams = ["arizona-cardinals",
	"atlanta-falcons",
	"baltimore-ravens",
	"buffalo-bills",
	"carolina-panthers",
	"chicago-bears",
	"cincinnati-bengals",
	"cleveland-browns",
	"dallas-cowboys",
	"denver-broncos",
	"detroit-lions",
	"green-bay-packers",
	"houston-texans",
	"indianapolis-colts",
	"jacksonville-jaguars",
	"kansas-city-chiefs",
	"los-angeles-chargers",
	"los-angeles-rams",
	"miami-dolphins",
	"minnesota-vikings",
	"new-england-patriots",
	"new-orleans-saints",
	"new-york-giants",
	"new-york-jets",
	"oakland-raiders",
	"philadelphia-eagles",
	"pittsburgh-steelers",
	"san-francisco-49ers",
	"seattle-seahawks",
	"tampabay-buccaneers",
	"tennessee-titans",
	"washington-redskins"
]

week = sys.argv[1]
print (week)

seasons = ["2017", "2018", "2019"]

weeks = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
"14", "15", "16", "17", "wild-card", "divisional", "conference", "super-bowl"]

pattern = re.compile("charts")

print ("Scraping images and html data...")
for team in teams:
#for team in ['baltimore-ravens']:
	#for season in seasons:
	for season in ["2019"]:
		print (team, "\t", season)
		for week in [week]:
			URL = "https://nextgenstats.nfl.com/charts/list/pass/" + team + "/" + season + "/" + week
			r = requests.get(URL)
			print (URL)
			
			soup = BeautifulSoup(r.content, "html.parser")

			script = soup.find_all("script", text=pattern)
			
			contains_charts = json.loads(str(script[0])[33:-131])
			
			
			if (len(contains_charts["charts"]["charts"]) != 0):
				for chart in contains_charts["charts"]['charts']['charts']:

					name = chart["lastName"] + "_" + chart["firstName"] + "_" + chart["position"]
					chart["team"] = team

					folder = str("Pass_Charts" + os.sep + team + os.sep + season + os.sep + week + os.sep)
					img_folder = folder + "images" + os.sep
					data_folder = folder + "data" + os.sep

					if not os.path.exists(img_folder):
						os.makedirs(img_folder)
					if not os.path.exists(data_folder):
						os.makedirs(data_folder)

					img_file = img_folder + name + ".jpeg"
					url = "https:" + chart["extraLargeImg"]
					urllib.request.urlretrieve(url, img_file)

					data_file = data_folder + name + ".txt"
					with open(data_file, 'w') as datafile: 
						json.dump(chart, datafile)
			else:
				print ("No chart\n")

print ("Done.")
