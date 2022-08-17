import React, {useState, useRef} from 'react'
import {GoogleMap, Marker} from '@react-google-maps/api';

const onLoad = marker => {
    console.log('marker: ', marker)
}

function GeoDistributionMap({initLat, initLng, initZoom, width, height, points}) {
    const mapContainerStyle = {
        height: height + "px",
        width: width + "px"
    }

    // Handle dragging of the map without automatically recentering it on rerender
    // From https://stackoverflow.com/questions/61624547/can-i-set-a-default-center-value-in-react-googe-maps-api
    const mapRef = useRef(null);
    const [position, setPosition] = useState({
        lat: initLat,
        lng: initLng
    });

    function handleLoad(map) {
        mapRef.current = map;
    }

    function handleCenter() {
        if (!mapRef.current) return;

        const newPos = mapRef.current.getCenter().toJSON();
        setPosition(newPos);
    }

    return (
        <GoogleMap
            id="result-map"
            mapContainerStyle={mapContainerStyle}
            zoom={initZoom}
            onLoad={handleLoad}
            onDragEnd={handleCenter}
            center={position}
        >
            {
                points ? (
                    points.map((point) => {
                        const pointPos = {
                            lat: point.latitude,
                            lng: point.longitude
                        }

                        return (
                            <Marker
                                key={point.key}
                                position={pointPos}
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
