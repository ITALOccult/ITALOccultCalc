using System;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class ShapeModels : IComparable
	{
		private string asteroidNumber = "";

		private string asteroidID = "";

		private string starID = "";

		private string source = "";

		private string iD = "";

		private string versionInfo = "";

		public static string[] Quality = new string[8] { "Not fitted", "Bad occn data", "Model wrong", "Minimum diameter", "Diameter but no fit", "Poor fit", "Good fit", "Not constrained" };

		private double surface_VolRatio = 1.0;

		private double phaseCorrection;

		private double volumeDia_Min;

		private double volumeDia_Max;

		private int year = 1900;

		private int month = 1;

		private int day = 1;

		private int chords;

		public static int SortField = 3;

		private int fitQuality;

		private int fitQualityForSort;

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

		public int DateForSort => year * 10000 + month * 100 + day;

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

		public string Source
		{
			get
			{
				return source;
			}
			set
			{
				source = value;
			}
		}

		public string ID
		{
			get
			{
				return iD;
			}
			set
			{
				iD = value;
			}
		}

		public double Surface_VolRatio
		{
			get
			{
				return surface_VolRatio;
			}
			set
			{
				surface_VolRatio = value;
			}
		}

		public int FitQuality
		{
			get
			{
				return fitQuality;
			}
			set
			{
				fitQuality = value;
				if ((fitQuality >= 3) & (fitQuality < 7))
				{
					fitQualityForSort = FitQuality + 1;
				}
				else if (fitQuality == 7)
				{
					fitQualityForSort = 3;
				}
				else
				{
					fitQualityForSort = fitQuality;
				}
			}
		}

		public int FitQualityForSort => fitQualityForSort;

		public double PhaseCorrection
		{
			get
			{
				return phaseCorrection;
			}
			set
			{
				phaseCorrection = value;
			}
		}

		public double VolumeDia_Min
		{
			get
			{
				return volumeDia_Min;
			}
			set
			{
				volumeDia_Min = value;
			}
		}

		public double VolumeDia_Max
		{
			get
			{
				return volumeDia_Max;
			}
			set
			{
				volumeDia_Max = value;
			}
		}

		public string VersionInfo
		{
			get
			{
				return versionInfo;
			}
			set
			{
				versionInfo = value;
			}
		}

		public int Chords
		{
			get
			{
				return chords;
			}
			set
			{
				chords = value;
			}
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (DateForSort.CompareTo(((ShapeModels)other).DateForSort) == 0)
				{
					if ((Source.Trim() == "") & (((ShapeModels)other).Source.Trim() == ""))
					{
						return 0;
					}
					if (Source.Trim() == "")
					{
						return 1;
					}
					if (((ShapeModels)other).Source.Trim() == "")
					{
						return -1;
					}
					if (Source.CompareTo(((ShapeModels)other).Source) == 0)
					{
						return ID.CompareTo(((ShapeModels)other).ID);
					}
					return Source.CompareTo(((ShapeModels)other).Source);
				}
				return DateForSort.CompareTo(((ShapeModels)other).DateForSort);
			case 1:
				if ((Source.Trim() == "") & (((ShapeModels)other).Source.Trim() == ""))
				{
					return 0;
				}
				if (Source.Trim() == "")
				{
					return 1;
				}
				if (((ShapeModels)other).Source.Trim() == "")
				{
					return -1;
				}
				if (Source.CompareTo(((ShapeModels)other).Source) == 0)
				{
					if (ID.CompareTo(((ShapeModels)other).ID) == 0)
					{
						return -FitQualityForSort.CompareTo(((ShapeModels)other).FitQualityForSort);
					}
					return ID.CompareTo(((ShapeModels)other).ID);
				}
				return Source.CompareTo(((ShapeModels)other).Source);
			case 2:
				if (VolumeDia_Max.CompareTo(((ShapeModels)other).VolumeDia_Max) == 0)
				{
					if (VolumeDia_Max == 0.0)
					{
						if (VolumeDia_Min.CompareTo(((ShapeModels)other).VolumeDia_Min) == 0)
						{
							return DateForSort.CompareTo(((ShapeModels)other).DateForSort);
						}
						return -VolumeDia_Min.CompareTo(((ShapeModels)other).VolumeDia_Min);
					}
					if (DateForSort.CompareTo(((ShapeModels)other).DateForSort) == 0)
					{
						if ((Source.Trim() == "") & (((ShapeModels)other).Source.Trim() == ""))
						{
							return 0;
						}
						if (Source.Trim() == "")
						{
							return 1;
						}
						if (((ShapeModels)other).Source.Trim() == "")
						{
							return -1;
						}
						if (Source.CompareTo(((ShapeModels)other).Source) == 0)
						{
							return ID.CompareTo(((ShapeModels)other).ID);
						}
						return Source.CompareTo(((ShapeModels)other).Source);
					}
					return DateForSort.CompareTo(((ShapeModels)other).DateForSort);
				}
				return -VolumeDia_Max.CompareTo(((ShapeModels)other).VolumeDia_Max);
			case 3:
				if (VolumeDia_Min.CompareTo(((ShapeModels)other).VolumeDia_Min) == 0)
				{
					if (DateForSort.CompareTo(((ShapeModels)other).DateForSort) == 0)
					{
						if ((Source.Trim() == "") & (((ShapeModels)other).Source.Trim() == ""))
						{
							return 0;
						}
						if (Source.Trim() == "")
						{
							return 1;
						}
						if (((ShapeModels)other).Source.Trim() == "")
						{
							return -1;
						}
						if (Source.CompareTo(((ShapeModels)other).Source) == 0)
						{
							return ID.CompareTo(((ShapeModels)other).ID);
						}
						return Source.CompareTo(((ShapeModels)other).Source);
					}
					return DateForSort.CompareTo(((ShapeModels)other).DateForSort);
				}
				return -VolumeDia_Min.CompareTo(((ShapeModels)other).VolumeDia_Min);
			case 4:
				if (FitQualityForSort.CompareTo(((ShapeModels)other).FitQualityForSort) == 0)
				{
					if ((Source.Trim() == "") & (((ShapeModels)other).Source.Trim() == ""))
					{
						return 0;
					}
					if (Source.Trim() == "")
					{
						return 1;
					}
					if (((ShapeModels)other).Source.Trim() == "")
					{
						return -1;
					}
					if (Source.CompareTo(((ShapeModels)other).Source) == 0)
					{
						return ID.CompareTo(((ShapeModels)other).ID);
					}
					return Source.CompareTo(((ShapeModels)other).Source);
				}
				return -FitQualityForSort.CompareTo(((ShapeModels)other).FitQualityForSort);
			default:
				return 0;
			}
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(" " + asteroidNumber + " " + asteroidID);
			stringBuilder.AppendFormat("|  {0,4:F0} ", year);
			stringBuilder.Append(Utilities.ShortMonths[month]);
			stringBuilder.AppendFormat("{0,3:F0}     ", day);
			stringBuilder.Append(StarID.PadRight(35));
			stringBuilder.Append(Source.ToString().PadRight(5) + "#" + ID.ToString().PadRight(8) + VersionInfo);
			stringBuilder.AppendFormat("|Min dia = {0,4:F0}km,  Max dia = {1,4:f0}km,  Phs Corrn = {2,4:f0}°, ", VolumeDia_Min, VolumeDia_Max, PhaseCorrection);
			stringBuilder.Append("  Fit quality = " + Quality[FitQuality]);
			return stringBuilder.ToString();
		}

		public string ShapeModelDetails()
		{
			if (Source == "")
			{
				return "No shape model has been fitted to these events";
			}
			string text = Source.ToString() + " #" + ID.ToString().PadRight(6) + string.Format("Surf/Vol = {0,5:f3},  ", Surface_VolRatio);
			if (VersionInfo != "No shape model fitted")
			{
				text = text + "Version = " + VersionInfo;
			}
			return text;
		}

		public string FitDetails()
		{
			return "Fit quality = " + Quality[FitQuality];
		}

		internal string EventDate()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendFormat("{0,4:F0} ", year);
			stringBuilder.Append(Utilities.ShortMonths[month]);
			stringBuilder.AppendFormat("{0,3:F0}", day);
			return stringBuilder.ToString();
		}

		public string SolutionLine_NoModel()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(EventDate());
			if (VolumeDia_Min > 30.0)
			{
				stringBuilder.AppendFormat(",{0,4:f0},{1,4:f0},  ", VolumeDia_Min, VolumeDia_Max);
			}
			else if (Source == "ISAM")
			{
				stringBuilder.Append(",    ,    ,  ");
			}
			else
			{
				stringBuilder.AppendFormat(",{0,4:f1},{1,4:f1},  ", VolumeDia_Min, VolumeDia_Max);
			}
			stringBuilder.Append(PhaseCorrection.ToString().PadLeft(4) + "°");
			if (FitQuality == 0)
			{
				stringBuilder.Append(", " + Chords + " chords");
			}
			stringBuilder.Append(", " + Quality[FitQuality]);
			return stringBuilder.ToString();
		}

		public string SolutionLine_WithModel()
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (Source.Trim() == "")
			{
				stringBuilder.Append("".PadRight(13));
			}
			else
			{
				stringBuilder.Append(Source.ToString().PadRight(5) + " #" + ID.ToString().PadRight(6));
			}
			if (VolumeDia_Min > 30.0)
			{
				stringBuilder.AppendFormat(",{0,4:f0},{1,4:f0},  ", VolumeDia_Min, VolumeDia_Max);
			}
			else if (Source == "ISAM")
			{
				stringBuilder.Append(",    ,    ,  ");
			}
			else
			{
				stringBuilder.AppendFormat(",{0,4:f1},{1,4:f1},  ", VolumeDia_Min, VolumeDia_Max);
			}
			stringBuilder.Append(PhaseCorrection.ToString().PadLeft(4) + "°");
			if (FitQuality == 0)
			{
				stringBuilder.Append(", " + Chords + " chords");
			}
			stringBuilder.Append(", " + Quality[FitQuality]);
			return stringBuilder.ToString();
		}

		public string SolutionLine_WithDateModel()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(EventDate());
			if (Source.Trim() == "")
			{
				stringBuilder.Append(",".PadRight(15));
			}
			else
			{
				stringBuilder.Append(", " + Source.ToString().PadRight(5) + " #" + ID.ToString().PadRight(6));
			}
			if (VolumeDia_Min > 30.0)
			{
				stringBuilder.AppendFormat(",{0,4:f0},{1,4:f0},  ", VolumeDia_Min, VolumeDia_Max);
			}
			else
			{
				stringBuilder.AppendFormat(",{0,4:f1},{1,4:f1},  ", VolumeDia_Min, VolumeDia_Max);
			}
			stringBuilder.Append(PhaseCorrection.ToString().PadLeft(4) + "°");
			if (FitQuality == 0)
			{
				stringBuilder.Append(", " + Chords + " chords");
			}
			stringBuilder.Append(", " + Quality[FitQuality]);
			return stringBuilder.ToString();
		}
	}
}
