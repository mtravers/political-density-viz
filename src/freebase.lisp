http://www.freebase.com/view/location/us_county

[
  {
    "/common/topic/image": {
      "id": null,
      "limit": 1,
      "optional": true
    },
    "/location/location/area": null,
    "/location/location/containedby": [
      {
        "id": null,
        "index": null,
        "limit": 6,
        "name": null,
        "optional": true,
        "sort": "index",
        "type": "/location/location"
      }
    ],
    "/location/statistical_region/population": [
      {
        "id": null,
        "index": null,
        "limit": 6,
        "number": null,
        "optional": true,
        "sort": "index",
        "type": "/measurement_unit/dated_integer"
      }
    ],
    "id": null,
    "limit": 60,
    "name": null,
    "s0:type": [
      {
        "id": "/location/us_county",
        "link": [
          {
            "timestamp": [
              {
                "optional": true,
                "type": "/type/datetime",
                "value": null
              }
            ],
            "type": "/type/link"
          }
        ],
        "type": "/type/type"
      }
    ],
    "sort": "s0:type.link.timestamp.value",
    "type": "/location/us_county"
  }
]
