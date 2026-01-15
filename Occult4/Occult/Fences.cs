namespace Occult
{
	internal class Fences
	{
		private int fenceCount;

		private double longitude = 100.0;

		private double latitude = 100.0;

		private double altitude;

		public double Longitude
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

		public double Latitude
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

		public double Altitude
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
	}
}
