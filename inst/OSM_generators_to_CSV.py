#!/usr/bin/python

import urllib
import urllib2
from zipfile import ZipFile
from StringIO import StringIO

# Take care of silly unicode issues.  Why is ASCII the default?
import sys
reload(sys)  # need to reload
sys.setdefaultencoding('UTF8')

from lxml import etree
import sys

def makeStringFromListWithUniqueElements(itemList):
	return ', '.join(list(set(itemList)))

# This processes all of the keys and values for the tags into a simplified standard format
# This involves trying to get the fuel type from one of several tags and locating wikipedia links
# A "soup" is also created for entity matching purposes
def processTags(tags):
	# these are lists since there can be multiple values
	name = list()
	owner = list()
	fuel = list()
	wikipedia = list()
	soup = list() # every other conceivable fact that could ever be useful in matching
	allKeyValuePairs = "" # a single string holding all the key/value pairs

	processedTagInfo = {}
	for tag in tags:
		key = tag.xpath("./@k")[0]
		value = tag.xpath("./@v")[0]
		#print key + "\t" + value
		if key == "generator:source" or key == "power_source" or key == "generator:method":
			fuel.append(value)
		elif key == "name" or "name:" in key:
			name.append(value)
		elif key == "wikipedia" or "wikipedia:" in key:
			wikipedia.append(value)

		# always write to the soup and allKeyValuePairs
		if not "xmlns" in key and not "xmlns" in value: # there's xml in some of the values, examples can by found by looking for www.topografix.com
			# also see http://www.openstreetmap.org/browse/node/1711161202
			soup.append(value)
			if not allKeyValuePairs:	# first value
				allKeyValuePairs = "|".join([key, value])
			else: 	# more than one key/value pair encountered, use || delimiter
				allKeyValuePairs = "||".join([allKeyValuePairs, "|".join([key, value])])

	# make unique
	processedTagInfo["name"] = makeStringFromListWithUniqueElements(name)
	processedTagInfo["owner"] = makeStringFromListWithUniqueElements(owner)
	processedTagInfo["fuel"] = makeStringFromListWithUniqueElements(fuel)
	processedTagInfo["wikipedia"] = makeStringFromListWithUniqueElements(wikipedia)
	processedTagInfo["soup"] = makeStringFromListWithUniqueElements(soup)
	processedTagInfo["allKeyValuePairs"] = allKeyValuePairs
	return processedTagInfo


# download latest file from Enipedia
url = urllib2.urlopen("http://enipedia.tudelft.nl/OpenStreetMap/PlanetPowerGenerators.zip")
zipfile = ZipFile(StringIO(url.read()))

context = etree.iterparse(zipfile.open("PlanetPowerGenerators.osm"), events=('end',), tag=('node', 'way', 'relation'))

# several different files are written to - the data will be joined together later in R code
with open("nodes_and_tags.csv", "w") as f_nodes_and_tags:
	f_nodes_and_tags.write("objectID\tlat\tlon\tname\towner\tfuel\twikipedia\tsoup\tallKeyValuePairs\n")

with open("ways_and_tags.csv", "w") as f_ways_and_tags:
	f_ways_and_tags.write("objectID\tname\towner\tfuel\twikipedia\tsoup\tallKeyValuePairs\n")

with open("ways_and_nodes.csv", "w") as f_ways_and_nodes:
	f_ways_and_nodes.write("objectID\tnode\n")

with open("relations_and_tags.csv", "w") as f_relations_and_tags:
	f_relations_and_tags.write("objectID\tname\towner\tfuel\twikipedia\tsoup\tallKeyValuePairs\n")

with open("relations_and_nodes.csv", "w") as f_relations_and_nodes:
	f_relations_and_nodes.write("objectID\tnode\n")

with open("relations_and_ways.csv", "w") as f_relations_and_ways:
	f_relations_and_ways.write("objectID\tway\n")


for action, elem in context:
        type = elem.tag

	# create a table for ways and tags
	# create another table for ways and nodes

        if type in ('node'):
		objectID = elem.xpath("./@id")[0]
		lat = elem.xpath("./@lat")[0]
		lon = elem.xpath("./@lon")[0]
		tags = elem.xpath("./tag")

		processedTagInfo = processTags(tags)

		with open("nodes_and_tags.csv", "a") as f_nodes_and_tags:
			f_nodes_and_tags.write(objectID + "\t" + lat + "\t" + lon + "\t" + processedTagInfo["name"] + "\t" + processedTagInfo["owner"] + "\t" + processedTagInfo["fuel"] + "\t" + processedTagInfo["wikipedia"] + "\t" + processedTagInfo["soup"] + "\t" + processedTagInfo["allKeyValuePairs"] + "\n")

        elif type in ('way'):
		objectID = elem.xpath("./@id")[0]
		nodes = elem.xpath("./nd/@ref")
		tags = elem.xpath("./tag")
		
		processedTagInfo = processTags(tags)

		# node and tag information is sent to two separate files

		with open("ways_and_tags.csv", "a") as f_ways_and_tags:
			f_ways_and_tags.write(objectID + "\t" + processedTagInfo["name"] + "\t" + processedTagInfo["owner"] + "\t" + processedTagInfo["fuel"] + "\t" + processedTagInfo["wikipedia"] + "\t" + processedTagInfo["soup"] + "\t" + processedTagInfo["allKeyValuePairs"] + "\n")

		for node in nodes:
			with open("ways_and_nodes.csv", "a") as f_ways_and_nodes:
				f_ways_and_nodes.write(objectID + "\t" + node + "\n")


        elif type in ('relation'):
		objectID = elem.xpath("./@id")[0]
		nodes = elem.xpath("./member[@type='node']/@ref")
		ways = elem.xpath("./member[@type='way']/@ref")
		tags = elem.xpath("./tag")

		processedTagInfo = processTags(tags)

		with open("relations_and_tags.csv", "a") as f_relations_and_tags:
			f_relations_and_tags.write(objectID + "\t" + processedTagInfo["name"] + "\t" + processedTagInfo["owner"] + "\t" + processedTagInfo["fuel"] + "\t" + processedTagInfo["wikipedia"] + "\t" + processedTagInfo["soup"] + "\t" + processedTagInfo["allKeyValuePairs"] + "\n")

		for node in nodes:
			with open("relations_and_nodes.csv", "a") as f_relations_and_nodes:
				f_relations_and_nodes.write(objectID + "\t" + node + "\n")

		for way in ways:
			with open("relations_and_ways.csv", "a") as f_relations_and_ways:
				f_relations_and_ways.write(objectID + "\t" + way + "\n")

        elem.clear()

        while elem.getprevious() is not None:
                del elem.getparent()[0]

