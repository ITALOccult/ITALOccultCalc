import xml.etree.ElementTree as ET
import sys

def xml_to_kml(xml_file, kml_file):
    try:
        tree = ET.parse(xml_file)
        root = tree.getroot()
        
        event = root.find('.//Event')
        if event is None:
            print("No event found in XML.")
            return

        asteroid_name = "Asteroid"
        obj = event.find('Object')
        if obj is not None:
            parts = obj.text.split(',')
            if len(parts) >= 2:
                asteroid_name = f"({parts[0]}) {parts[1]}"

        kml = [
            '<?xml version="1.0" encoding="UTF-8"?>',
            '<kml xmlns="http://www.opengis.net/kml/2.2">',
            '  <Document>',
            f'    <name>Occultation Path: {asteroid_name}</name>',
            '    <Style id="centerLine">',
            '      <LineStyle><color>ff000000</color><width>3</width></LineStyle>',
            '    </Style>',
            '    <Style id="limitLine">',
            '      <LineStyle><color>ff0000ff</color><width>1</width></LineStyle>',
            '    </Style>'
        ]

        # Center Line
        center_line = event.find('CenterLine')
        if center_line is not None:
            kml.append('    <Placemark>')
            kml.append('      <name>Center Line</name>')
            kml.append('      <styleUrl>#centerLine</styleUrl>')
            kml.append('      <LineString><coordinates>')
            for pt in center_line.findall('Point'):
                lat = pt.find('Latitude').text
                lon = pt.find('Longitude').text
                kml.append(f'{lon},{lat},0 ')
            kml.append('      </coordinates></LineString>')
            kml.append('    </Placemark>')

        # North Limit
        north_limit = event.find('NorthLimit')
        if north_limit is not None:
            kml.append('    <Placemark>')
            kml.append('      <name>North Limit (1-sigma)</name>')
            kml.append('      <styleUrl>#limitLine</styleUrl>')
            kml.append('      <LineString><coordinates>')
            for pt in north_limit.findall('Point'):
                lat = pt.find('Latitude').text
                lon = pt.find('Longitude').text
                kml.append(f'{lon},{lat},0 ')
            kml.append('      </coordinates></LineString>')
            kml.append('    </Placemark>')

        # South Limit
        south_limit = event.find('SouthLimit')
        if south_limit is not None:
            kml.append('    <Placemark>')
            kml.append('      <name>South Limit (1-sigma)</name>')
            kml.append('      <styleUrl>#limitLine</styleUrl>')
            kml.append('      <LineString><coordinates>')
            for pt in south_limit.findall('Point'):
                lat = pt.find('Latitude').text
                lon = pt.find('Longitude').text
                kml.append(f'{lon},{lat},0 ')
            kml.append('      </coordinates></LineString>')
            kml.append('    </Placemark>')

        kml.append('  </Document>')
        kml.append('</kml>')

        with open(kml_file, 'w') as f:
            f.write('\n'.join(kml))
        print(f"✅ KML generated: {kml_file}")

    except Exception as e:
        print(f"❌ Error generating KML: {e}")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python3 xml_to_kml.py <input.xml> <output.kml>")
    else:
        xml_to_kml(sys.argv[1], sys.argv[2])
