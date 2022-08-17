import React from 'react'
import {GoogleMap, Marker} from '@react-google-maps/api';

const position = {
    lat: 37.772,
    lng: -122.214
}

const onLoad = marker => {
    console.log('marker: ', marker)
}

function GeoDistributionMap({initLat, initLng, initZoom, width, height, points}) {
    const mapContainerStyle = {
        height: height + "px",
        width: width + "px"
    }

    const center = {
        lat: initLat,
        lng: initLng
    }

    return (
        <GoogleMap
            id="result-map"
            mapContainerStyle={mapContainerStyle}
            zoom={initZoom}
            center={center}
        >
            {
                points ? (
                    points.map((point) => {
                        const position = {
                            lat: point.latitude,
                            lng: point.longitude
                        }

                        return (
                            <Marker
                                key={point.key}
                                position={position}
                                icon={point.icon || 'speech/red_dot.png'}
                                title={point.label}
                            />
                        )
                    })
                ) : null
            }
        </GoogleMap>
    )
}

export default React.memo(GeoDistributionMap)
