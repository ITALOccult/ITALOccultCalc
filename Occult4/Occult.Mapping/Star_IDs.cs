using System;

namespace Occult.Mapping
{
	internal class Star_IDs : IComparable
	{
		private static int[] Greek = new int[25]
		{
			0, 1, 2, 7, 4, 5, 26, 8, 17, 9,
			11, 12, 13, 14, 24, 15, 16, 18, 19, 20,
			21, 6, 3, 25, 23
		};

		private double raStar;

		private double decStar;

		private int bayerNumber;

		private int flamsteedNumber;

		internal double RAStar
		{
			get
			{
				return raStar;
			}
			set
			{
				raStar = value;
			}
		}

		internal double DecStar
		{
			get
			{
				return decStar;
			}
			set
			{
				decStar = value;
			}
		}

		internal int BayerNumber
		{
			get
			{
				return bayerNumber;
			}
			set
			{
				bayerNumber = value;
			}
		}

		internal int FlamsteedNumber
		{
			get
			{
				return flamsteedNumber;
			}
			set
			{
				flamsteedNumber = value;
			}
		}

		internal string Star_identifier
		{
			get
			{
				string text = "";
				if (BayerNumber > 0)
				{
					text += Utilities.GreekUnicode[BayerNumber];
					if (FlamsteedNumber > 0)
					{
						text += FlamsteedNumber;
					}
				}
				else if (FlamsteedNumber > 0)
				{
					text += FlamsteedNumber;
				}
				return text;
			}
		}

		internal string Star_identifierWithConstellation
		{
			get
			{
				string star_identifier = Star_identifier;
				if (star_identifier.Length > 0)
				{
					return star_identifier + " " + Utilities.Constellation(RAStar / (180.0 / Math.PI), DecStar / (180.0 / Math.PI), AbbreviatedName: true);
				}
				return "";
			}
		}

		public int CompareTo(object other)
		{
			return RAStar.CompareTo(((Star_IDs)other).RAStar);
		}
	}
}
