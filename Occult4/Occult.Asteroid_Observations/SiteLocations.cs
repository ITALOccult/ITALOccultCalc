using System;

namespace Occult.Asteroid_Observations
{
	internal class SiteLocations : IComparable
	{
		private double longitude;

		private double latitude;

		private string site;

		private int siteCount;

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

		public int SiteCount
		{
			get
			{
				return siteCount;
			}
			set
			{
				siteCount = value;
			}
		}

		public string Site
		{
			get
			{
				return site;
			}
			set
			{
				site = value;
			}
		}

		public int CompareTo(object other)
		{
			return Longitude.CompareTo(((SiteLocations)other).Longitude);
		}
	}
}
