using System;
using System.Text;

namespace Occult
{
	internal class PlanetContactTimes : IComparable
	{
		public static int SortField;

		private double longitude = -300.0;

		private double latitude = 100.0;

		private string siteName;

		private string times;

		public string SiteName
		{
			get
			{
				return siteName;
			}
			set
			{
				siteName = value;
			}
		}

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

		public string Times
		{
			get
			{
				return times;
			}
			set
			{
				times = value;
			}
		}

		public int CompareTo(object other)
		{
			return SortField switch
			{
				0 => siteName.CompareTo(((PlanetContactTimes)other).SiteName), 
				1 => longitude.CompareTo(((PlanetContactTimes)other).Longitude), 
				2 => ((PlanetContactTimes)other).Latitude.CompareTo(latitude), 
				_ => 0, 
			};
		}

		public override string ToString()
		{
			return ToString(Abbreviated: true);
		}

		public string ToString(bool Abbreviated)
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,-32} ", siteName);
			if (latitude < 100.0)
			{
				string text = Utilities.DEGtoDMS(longitude, 4, 1, MinutesOnly: true).PadRight(10);
				string text2 = Utilities.DEGtoDMS(latitude, 3, 1, MinutesOnly: true).PadRight(9);
				if (Abbreviated)
				{
					text = text.Remove(6, 3).Insert(6, "5. ");
					text2 = text2.Remove(5, 3).Insert(5, "5. ");
				}
				stringBuilder.Append(text);
				stringBuilder.Append(text2);
				stringBuilder.Append(times);
			}
			return stringBuilder.ToString();
		}
	}
}
