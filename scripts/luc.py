#!/usr/bin/env python

"""

SOME METADATA HERE

"""

import ee
from ee import batch
from pprint import pprint
import webbrowser
import folium


# Define a method for displaying Earth Engine image tiles to folium map.
def add_ee_layer(self, eeImageObject, visParams, name):
  mapID = ee.Image(eeImageObject).getMapId(visParams)
  folium.raster_layers.TileLayer(
    tiles = "https://earthengine.googleapis.com/map/"+mapID['mapid']+
      "/{z}/{x}/{y}?token="+mapID['token'],
    attr = "Map Data © Google Earth Engine",
    name = name,
    overlay = True,
    control = True
  ).add_to(self)

# Add EE drawing method to folium.
folium.Map.add_ee_layer = add_ee_layer

ee.Initialize()

giri = ee.ImageCollection("LANDSAT/MANGROVE_FORESTS");
giri_img = giri.first()

sentinel = ee.ImageCollection("COPERNICUS/S2")
sentinel_img = sentinel.first()
sentinel2plot = sentinel_img.mask(giri_img);


myMap = folium.Map(location=[8, 100], zoom_start=7)
myMap.add_ee_layer(giri_img, {}, 'Mangroves')
myMap.add_ee_layer(sentinel2plot, {'bands': ['B4', 'B3', 'B2'],}, 'Sentinel')

myMap.add_child(folium.LayerControl())

myMap.save("myMap.html")
webbrowser.open('./myMap.html')

