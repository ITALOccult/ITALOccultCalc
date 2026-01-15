namespace Occult
{
	internal class GoogleMapsCoords
	{
		private int seqnum;

		private double longitude;

		private double latitude;

		private double altitude;

		private double ge_altitude;

		private double resolution;

		private string observer = "";

		internal int SeqNum
		{
			get
			{
				return seqnum;
			}
			set
			{
				seqnum = value;
			}
		}

		internal double Longitude
		{
			get
			{
				return longitude;
			}
			set
			{
				longitude = value;
			}
		}

		internal double Latitude
		{
			get
			{
				return latitude;
			}
			set
			{
				latitude = value;
			}
		}

		internal double Altitude
		{
			get
			{
				return altitude;
			}
			set
			{
				altitude = value;
			}
		}

		internal double GE_Altitude
		{
			get
			{
				return ge_altitude;
			}
			set
			{
				ge_altitude = value;
			}
		}

		internal double Resolution
		{
			get
			{
				return resolution;
			}
			set
			{
				resolution = value;
			}
		}

		internal string Observer
		{
			get
			{
				return observer;
			}
			set
			{
				observer = value;
			}
		}
	}
}
