using System;

namespace Occult.Eclipses
{
	internal class CraterCoords : IComparable
	{
		private string name = "";

		private double lat;

		private double lon;

		private double radiusKm;

		internal string Name
		{
			get
			{
				return name;
			}
			set
			{
				name = value;
			}
		}

		internal double Lon
		{
			get
			{
				return lon;
			}
			set
			{
				lon = value;
			}
		}

		internal double Lat
		{
			get
			{
				return lat;
			}
			set
			{
				lat = value;
			}
		}

		internal double RadiusKm
		{
			get
			{
				return radiusKm;
			}
			set
			{
				radiusKm = value;
			}
		}

		public int CompareTo(object other)
		{
			return Name.CompareTo(((CraterCoords)other).Name);
		}
	}
}
