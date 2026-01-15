using System;

namespace Occult.Asteroid_Observations
{
	internal class Asteroid_Observations_StarMatch : IComparable
	{
		internal static bool SortByStarNumber = true;

		private double ra;

		private double dec;

		private double mag;

		private string description;

		private string date;

		private string source;

		private string temp;

		private string id = "";

		public double RA
		{
			get
			{
				return ra;
			}
			set
			{
				ra = value;
			}
		}

		public double Dec
		{
			get
			{
				return dec;
			}
			set
			{
				dec = value;
			}
		}

		public string Description
		{
			get
			{
				return description;
			}
			set
			{
				temp = value;
				if (temp.Contains("HIP"))
				{
					do
					{
						if (temp.Length < 52)
						{
							temp = temp.Replace("HIP", "HIP ");
						}
					}
					while (temp.Length < 52);
				}
				description = temp;
			}
		}

		public string Date
		{
			get
			{
				return date;
			}
			set
			{
				date = value;
			}
		}

		public string Source
		{
			set
			{
				source = value;
			}
		}

		public string ID
		{
			get
			{
				return id;
			}
			set
			{
				id = value;
			}
		}

		public double Mag
		{
			set
			{
				mag = value;
			}
		}

		public string DescriptionWithSource => description.Replace("\r\n", "").PadRight(54) + string.Format("{0,4:f1}  ", mag) + source.PadRight(11) + ID + "\r\n";

		public int CompareTo(object other)
		{
			if (SortByStarNumber)
			{
				return description.Substring(40).CompareTo(((Asteroid_Observations_StarMatch)other).description.Substring(40));
			}
			return ra.CompareTo(((Asteroid_Observations_StarMatch)other).ra);
		}
	}
}
