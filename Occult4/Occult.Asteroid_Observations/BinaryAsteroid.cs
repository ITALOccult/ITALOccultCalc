using System;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class BinaryAsteroid : IComparable
	{
		private string asteroidNumber = "";

		private string asteroidID = "";

		private string starID = "";

		private string satelliteID = "";

		private string cbet = "";

		private double sep;

		private double pA;

		private double hour;

		private double majorAxisCompanion;

		private double minorAxisCompanion;

		private double pAMajorAxisCompanion;

		private int year = 1900;

		private int month = 1;

		private int day = 1;

		public static int SortField = 5;

		private int fitQualityCompanion;

		public string AsteroidNumber
		{
			get
			{
				return asteroidNumber;
			}
			set
			{
				asteroidNumber = value;
			}
		}

		public string AsteroidID
		{
			get
			{
				return asteroidID;
			}
			set
			{
				asteroidID = value;
			}
		}

		public string SatelliteID
		{
			get
			{
				return satelliteID;
			}
			set
			{
				satelliteID = value;
			}
		}

		public string StarID
		{
			get
			{
				return starID;
			}
			set
			{
				starID = value;
			}
		}

		public double PA
		{
			get
			{
				if (pA < 0.0)
				{
					return pA + 360.0;
				}
				return pA;
			}
			set
			{
				pA = value;
			}
		}

		public double Sep
		{
			get
			{
				return sep;
			}
			set
			{
				sep = value;
			}
		}

		public double MajorAxisCompanion
		{
			get
			{
				return majorAxisCompanion;
			}
			set
			{
				majorAxisCompanion = value;
			}
		}

		public double MinorAxisCompanion
		{
			get
			{
				return minorAxisCompanion;
			}
			set
			{
				minorAxisCompanion = value;
			}
		}

		public double PAMajorAxisCompanion
		{
			get
			{
				if (pAMajorAxisCompanion < 0.0)
				{
					return pAMajorAxisCompanion + 360.0;
				}
				return pAMajorAxisCompanion;
			}
			set
			{
				pAMajorAxisCompanion = value;
			}
		}

		public int Year
		{
			get
			{
				return year;
			}
			set
			{
				year = value;
			}
		}

		public int Month
		{
			get
			{
				return month;
			}
			set
			{
				month = value;
			}
		}

		public int Day
		{
			get
			{
				return day;
			}
			set
			{
				day = value;
			}
		}

		public double Hour
		{
			get
			{
				return hour;
			}
			set
			{
				hour = value;
			}
		}

		public int DateForSort => year * 10000 + month * 100 + day;

		public int FitQualityCompanion
		{
			get
			{
				return fitQualityCompanion;
			}
			set
			{
				fitQualityCompanion = value;
			}
		}

		public string CBET
		{
			get
			{
				return cbet;
			}
			set
			{
				cbet = value;
			}
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (AsteroidID.CompareTo(((BinaryAsteroid)other).AsteroidID) == 0)
				{
					return DateForSort.CompareTo(((BinaryAsteroid)other).DateForSort);
				}
				return AsteroidID.CompareTo(((BinaryAsteroid)other).AsteroidID);
			case 1:
				if (AsteroidNumber.CompareTo(((BinaryAsteroid)other).AsteroidNumber) == 0)
				{
					return DateForSort.CompareTo(((BinaryAsteroid)other).DateForSort);
				}
				return AsteroidNumber.CompareTo(((BinaryAsteroid)other).AsteroidNumber);
			case 2:
				return -DateForSort.CompareTo(((BinaryAsteroid)other).DateForSort);
			case 3:
				return Sep.CompareTo(((BinaryAsteroid)other).Sep);
			case 4:
				return MajorAxisCompanion.CompareTo(((BinaryAsteroid)other).MajorAxisCompanion);
			case 5:
				if (CBET.CompareTo(((BinaryAsteroid)other).CBET) == 0)
				{
					return -DateForSort.CompareTo(((BinaryAsteroid)other).DateForSort);
				}
				return -CBET.CompareTo(((BinaryAsteroid)other).CBET);
			default:
				return 0;
			}
		}

		public override string ToString()
		{
			string[] array = new string[5] { "None", "Is it a Satellite?", "Approx offset", "Offset + size", "Offset + shape" };
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(" " + asteroidNumber + " " + asteroidID);
			if (SatelliteID.Length > 0)
			{
				stringBuilder.Append(" :   " + SatelliteID);
			}
			if (CBET.Length > 0)
			{
				stringBuilder.Append("|***   Satellite discovered by occultation. See CBET " + CBET + " ***");
			}
			stringBuilder.AppendFormat("|  {0,4:F0} ", year);
			stringBuilder.Append(Utilities.ShortMonths[month]);
			stringBuilder.AppendFormat("{0,9:F5}     ", (double)day + Hour / 24.0);
			stringBuilder.Append(StarID.PadRight(19));
			stringBuilder.AppendFormat("|Sepn {0,6:F1}mas,  PA {1,3:f0}°, ", Sep, PA);
			stringBuilder.AppendFormat(" Major {0,3:F0}km,  Minor {1,3:f0}km,  PA {2,3:f0}°", MajorAxisCompanion, MinorAxisCompanion, PAMajorAxisCompanion);
			stringBuilder.Append("|Fit quality for companion = " + array[FitQualityCompanion]);
			return stringBuilder.ToString();
		}
	}
}
