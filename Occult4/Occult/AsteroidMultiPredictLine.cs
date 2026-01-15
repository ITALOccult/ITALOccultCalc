using System;
using System.Text;

namespace Occult
{
	internal class AsteroidMultiPredictLine : IComparable
	{
		public static int SortField;

		private double distance;

		private double probability;

		private double longitude = -300.0;

		private double latitude = 100.0;

		private double siteAlt;

		private double uT;

		private double separation;

		private double starAlt;

		private double sunAlt;

		private string siteName;

		private string direction = "-";

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

		public double Distance
		{
			get
			{
				return distance;
			}
			set
			{
				distance = value;
			}
		}

		public string Direction
		{
			get
			{
				return direction;
			}
			set
			{
				direction = value;
			}
		}

		public double Probability
		{
			get
			{
				return probability;
			}
			set
			{
				probability = value;
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

		public double SiteAlt
		{
			get
			{
				return siteAlt;
			}
			set
			{
				siteAlt = value;
			}
		}

		public double UT
		{
			get
			{
				return uT;
			}
			set
			{
				uT = value;
			}
		}

		public double Separation
		{
			get
			{
				return separation;
			}
			set
			{
				separation = value;
			}
		}

		public double StarAlt
		{
			get
			{
				return starAlt;
			}
			set
			{
				starAlt = value;
			}
		}

		public double SunAlt
		{
			get
			{
				return sunAlt;
			}
			set
			{
				sunAlt = value;
			}
		}

		public override string ToString()
		{
			return ToString(Abbreviated: true);
		}

		public string ToString(bool Abbreviated)
		{
			string text = "";
			string text2 = "";
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,9:F1} ", distance);
			stringBuilder.Append(direction);
			stringBuilder.AppendFormat("{0,5:F0}%   ", probability);
			stringBuilder.AppendFormat("{0,-32}", siteName);
			if (latitude < 100.0)
			{
				text = Utilities.DEGtoDMS(longitude, 4, 0, MinutesOnly: false).PadRight(12);
				text2 = Utilities.DEGtoDMS(latitude, 3, 0, MinutesOnly: false).PadRight(11);
				if (Abbreviated)
				{
					text = Utilities.DEGtoDMS(longitude, 4, 1, MinutesOnly: false).PadRight(12);
					text2 = Utilities.DEGtoDMS(latitude, 3, 1, MinutesOnly: false).PadRight(11);
					text = text.Remove(6, 6).Insert(6, "5     ");
					text2 = text2.Remove(5, 6).Insert(5, "5     ");
				}
				stringBuilder.Append(text);
				stringBuilder.Append(text2);
				stringBuilder.AppendFormat("{0,5:F0}  ", siteAlt);
				if (uT < 0.0)
				{
					stringBuilder.Append("-" + Utilities.DEGtoDMS(uT + 24.0, 2, 0, MinutesOnly: false));
				}
				else if (uT >= 24.0)
				{
					stringBuilder.Append("+" + Utilities.DEGtoDMS(uT - 24.0, 2, 0, MinutesOnly: false));
				}
				else
				{
					stringBuilder.Append(" " + Utilities.DEGtoDMS(uT, 2, 0, MinutesOnly: false));
				}
				stringBuilder.AppendFormat("{0,9:F3}", separation);
				stringBuilder.AppendFormat("{0,5:F0}", starAlt);
				stringBuilder.AppendFormat("{0,5:F0}", sunAlt);
			}
			return stringBuilder.ToString();
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				return ((AsteroidMultiPredictLine)other).Distance.CompareTo(distance);
			case 1:
				return siteName.CompareTo(((AsteroidMultiPredictLine)other).SiteName);
			case 2:
				if (longitude == ((AsteroidMultiPredictLine)other).Longitude)
				{
					return distance.CompareTo(((AsteroidMultiPredictLine)other).Distance);
				}
				return longitude.CompareTo(((AsteroidMultiPredictLine)other).Longitude);
			case 3:
				if (latitude == ((AsteroidMultiPredictLine)other).Latitude)
				{
					return distance.CompareTo(((AsteroidMultiPredictLine)other).Distance);
				}
				return ((AsteroidMultiPredictLine)other).Latitude.CompareTo(latitude);
			case 4:
				return uT.CompareTo(((AsteroidMultiPredictLine)other).UT);
			default:
				return 0;
			}
		}
	}
}
