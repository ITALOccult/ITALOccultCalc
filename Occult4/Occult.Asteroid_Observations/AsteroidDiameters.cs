using System;
using System.Collections.Generic;
using System.Text;

namespace Occult.Asteroid_Observations
{
	internal class AsteroidDiameters : IComparable
	{
		private string asteroidNumber;

		private string asteroidID;

		private List<string> shapeModelSource;

		private List<string> shapeModelNum;

		private List<int> shapeModelDiameter;

		private List<string> shapeModelFit;

		private double x_Dia;

		private double y_Dia;

		private double pA;

		private int quality;

		private int numChords;

		private double iR_Dia;

		private double iR_Sedv;

		private double iRAS_Dia;

		private double iRAS_Sdev;

		private double akari_Dia;

		private double akari_Sdev;

		private double nEOWISE_dia;

		private double neOWISE_Sdev;

		private double ellipseDia;

		private double sdev_Major;

		private double sdev_Minor;

		private double sdev_PA;

		private double astNum;

		private int year;

		private int month;

		private int day;

		private bool solve_Circular;

		private bool solveUsingAssumedDiameter;

		private bool hasShapeModel;

		public static int SortField;

		public string AsteroidNumber
		{
			get
			{
				return asteroidNumber;
			}
			set
			{
				asteroidNumber = value;
				double.TryParse(value, out astNum);
			}
		}

		public double AsteroidNum => astNum;

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

		public double IRall_Dia
		{
			get
			{
				return iR_Dia;
			}
			set
			{
				iR_Dia = value;
			}
		}

		public double IRall_Sdev
		{
			get
			{
				return iR_Sedv;
			}
			set
			{
				iR_Sedv = value;
			}
		}

		public double IRAS_Dia
		{
			get
			{
				return iRAS_Dia;
			}
			set
			{
				iRAS_Dia = value;
			}
		}

		public double IRAS_Sdev
		{
			get
			{
				return iRAS_Sdev;
			}
			set
			{
				iRAS_Sdev = value;
			}
		}

		public double Akari_Dia
		{
			get
			{
				return akari_Dia;
			}
			set
			{
				akari_Dia = value;
			}
		}

		public double Akari_Sdev
		{
			get
			{
				return akari_Sdev;
			}
			set
			{
				akari_Sdev = value;
			}
		}

		public double NEOWISE_Dia
		{
			get
			{
				return nEOWISE_dia;
			}
			set
			{
				nEOWISE_dia = value;
			}
		}

		public double NEOWISE_Sdev
		{
			get
			{
				return neOWISE_Sdev;
			}
			set
			{
				neOWISE_Sdev = value;
			}
		}

		public double X_Dia
		{
			get
			{
				return x_Dia;
			}
			set
			{
				x_Dia = value;
			}
		}

		public double Y_Dia
		{
			get
			{
				return y_Dia;
			}
			set
			{
				y_Dia = value;
			}
		}

		public double EllipseDiameter
		{
			get
			{
				return Math.Sqrt(x_Dia * y_Dia);
			}
			set
			{
				ellipseDia = value;
			}
		}

		public double EllipseDiameterUncert => Math.Sqrt((sdev_Major * sdev_Major + sdev_Minor * sdev_Minor) / 2.0);

		public double PA
		{
			get
			{
				return pA;
			}
			set
			{
				pA = value;
			}
		}

		public double Sdev_Major
		{
			get
			{
				return sdev_Major;
			}
			set
			{
				sdev_Major = value;
			}
		}

		public double Sdev_Minor
		{
			get
			{
				return sdev_Minor;
			}
			set
			{
				sdev_Minor = value;
			}
		}

		public double Sdev_PA
		{
			get
			{
				return sdev_PA;
			}
			set
			{
				sdev_PA = value;
			}
		}

		public bool Solve_Circular
		{
			get
			{
				return solve_Circular;
			}
			set
			{
				solve_Circular = value;
			}
		}

		public int Quality
		{
			get
			{
				return quality;
			}
			set
			{
				quality = value;
			}
		}

		public int NumChords
		{
			get
			{
				return numChords;
			}
			set
			{
				numChords = value;
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

		public int SortDate => year * 10000 + month * 100 + day;

		public bool HasShapeModel
		{
			get
			{
				return hasShapeModel;
			}
			set
			{
				hasShapeModel = value;
			}
		}

		public bool SolveUsingAssumedDiameter
		{
			get
			{
				return solveUsingAssumedDiameter;
			}
			set
			{
				solveUsingAssumedDiameter = value;
			}
		}

		public List<string> ShapeModelSource
		{
			get
			{
				return shapeModelSource;
			}
			set
			{
				shapeModelSource = value;
			}
		}

		public List<string> ShapeModelNum
		{
			get
			{
				return shapeModelNum;
			}
			set
			{
				shapeModelNum = value;
			}
		}

		public List<int> ShapeModelDiameter
		{
			get
			{
				return shapeModelDiameter;
			}
			set
			{
				shapeModelDiameter = value;
			}
		}

		public List<string> ShapeModelFit
		{
			get
			{
				return shapeModelFit;
			}
			set
			{
				shapeModelFit = value;
			}
		}

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				return SortDate.CompareTo(((AsteroidDiameters)other).SortDate);
			case 1:
				if (Quality.CompareTo(((AsteroidDiameters)other).Quality) == 0)
				{
					return AsteroidNumber.CompareTo(((AsteroidDiameters)other).AsteroidNumber);
				}
				return ((AsteroidDiameters)other).Quality.CompareTo(Quality);
			case 2:
				if (AsteroidNumber.Contains("P") & !((AsteroidDiameters)other).AsteroidNumber.Contains("P"))
				{
					return 1;
				}
				if (!AsteroidNumber.Contains("P") & ((AsteroidDiameters)other).AsteroidNumber.Contains("P"))
				{
					return -1;
				}
				if (AsteroidNumber.Contains("P") & ((AsteroidDiameters)other).AsteroidNumber.Contains("P"))
				{
					return AsteroidNumber.CompareTo(((AsteroidDiameters)other).AsteroidNumber);
				}
				if (AsteroidNumber.CompareTo(((AsteroidDiameters)other).AsteroidNumber) == 0)
				{
					return SortDate.CompareTo(((AsteroidDiameters)other).SortDate);
				}
				return AsteroidNum.CompareTo(((AsteroidDiameters)other).AsteroidNum);
			case 3:
				if (AsteroidID.CompareTo(((AsteroidDiameters)other).AsteroidID) == 0)
				{
					return SortDate.CompareTo(((AsteroidDiameters)other).SortDate);
				}
				return AsteroidID.CompareTo(((AsteroidDiameters)other).AsteroidID);
			case 4:
				return ((AsteroidDiameters)other).X_Dia.CompareTo(X_Dia);
			case 5:
				if (HasShapeModel.CompareTo(((AsteroidDiameters)other).HasShapeModel) == 0)
				{
					if (AsteroidNumber.Contains("P") & !((AsteroidDiameters)other).AsteroidNumber.Contains("P"))
					{
						return 1;
					}
					if (!AsteroidNumber.Contains("P") & ((AsteroidDiameters)other).AsteroidNumber.Contains("P"))
					{
						return -1;
					}
					if (AsteroidNumber.Contains("P") & ((AsteroidDiameters)other).AsteroidNumber.Contains("P"))
					{
						return AsteroidNumber.CompareTo(((AsteroidDiameters)other).AsteroidNumber);
					}
					if (AsteroidNumber.CompareTo(((AsteroidDiameters)other).AsteroidNumber) == 0)
					{
						return SortDate.CompareTo(((AsteroidDiameters)other).SortDate);
					}
					return AsteroidNum.CompareTo(((AsteroidDiameters)other).AsteroidNum);
				}
				return -HasShapeModel.CompareTo(((AsteroidDiameters)other).HasShapeModel);
			default:
				return 0;
			}
		}

		public override string ToString()
		{
			return ToString(IsPDS: false);
		}

		public string ToString(bool IsPDS)
		{
			StringBuilder stringBuilder = new StringBuilder();
			if (!IsPDS)
			{
				stringBuilder.Append(asteroidNumber.PadLeft(7) + " " + asteroidID.PadRight(17).Substring(0, 17));
				stringBuilder.AppendFormat("  {0,4:F0}", year);
				stringBuilder.Append(" " + Utilities.ShortMonths[month]);
				stringBuilder.AppendFormat("{0,3:F0}: ", day);
				if (HasShapeModel)
				{
					stringBuilder.AppendFormat("y ");
				}
				else
				{
					stringBuilder.Append("  ");
				}
				if (!AsteroidNumber.Contains("P"))
				{
					stringBuilder.Append(string.Format(" {0,4:F0}", IRall_Dia));
				}
				else
				{
					stringBuilder.Append("     ");
				}
				stringBuilder.Append(string.Format(" |{0,8:F1}", x_Dia).Substring(0, 10));
			}
			else
			{
				stringBuilder.Append(asteroidNumber.PadLeft(7) + "|" + asteroidID.PadRight(17).Substring(0, 17));
				if (!AsteroidNumber.Contains("P"))
				{
					if (HasShapeModel)
					{
						stringBuilder.AppendFormat("|1");
					}
					else
					{
						stringBuilder.Append("|0");
					}
					stringBuilder.Append(string.Format("|{0,4:F0}", IRall_Dia));
					if (solve_Circular)
					{
						if (SolveUsingAssumedDiameter)
						{
							stringBuilder.Append("|2");
						}
						else
						{
							stringBuilder.Append("|1");
						}
					}
					else
					{
						stringBuilder.Append("|0");
					}
					stringBuilder.Append(string.Format("|{0,7:F1}", x_Dia).Substring(0, 8));
				}
				else
				{
					stringBuilder.Append(string.Format("|{0,7:F0}", IRall_Dia));
					if (solve_Circular)
					{
						if (SolveUsingAssumedDiameter)
						{
							stringBuilder.Append("|2");
						}
						else
						{
							stringBuilder.Append("|1");
						}
					}
					else
					{
						stringBuilder.Append("|0");
					}
					stringBuilder.Append(string.Format("|{0,7:F1}", x_Dia).Substring(0, 8));
				}
			}
			if (!IsPDS)
			{
				stringBuilder.AppendFormat(" {0,8:F1}", y_Dia);
				stringBuilder.AppendFormat("  {0,6:F1}", PA);
				double num = Math.Sqrt(x_Dia * y_Dia);
				stringBuilder.AppendFormat("{0,9:F1}", num);
				stringBuilder.AppendFormat("{0,6:F1}%", (num - IRall_Dia) / num * 100.0);
			}
			else
			{
				if ((Sdev_Major > 0.0) & (Sdev_Major < 99.95))
				{
					stringBuilder.AppendFormat("|{0,5:F1}", Sdev_Major);
				}
				else
				{
					stringBuilder.Append("|-99.9");
				}
				if (solve_Circular)
				{
					stringBuilder.Append("|-9999.9|-99.9|-999.9|-99.9");
				}
				else
				{
					stringBuilder.AppendFormat("|{0,7:F1}", y_Dia);
					if ((Sdev_Minor > 0.0) & (Sdev_Minor < 99.95))
					{
						stringBuilder.AppendFormat("|{0,5:F1}", Sdev_Minor);
					}
					else
					{
						stringBuilder.Append("|-99.9");
					}
					if (PA != 0.0)
					{
						stringBuilder.AppendFormat("|{0,5:F1}", PA);
					}
					else
					{
						stringBuilder.Append("|-99.9");
					}
					if ((Sdev_PA > 0.0) & (Sdev_PA < 50.0))
					{
						stringBuilder.AppendFormat("|{0,5:F1}", Sdev_PA);
					}
					else
					{
						stringBuilder.Append("|-99.9");
					}
				}
			}
			if (!IsPDS)
			{
				stringBuilder.AppendFormat("|{0,3:F0}", quality);
			}
			else
			{
				stringBuilder.AppendFormat("|{0,1:F0}", quality);
			}
			if (!IsPDS)
			{
				stringBuilder.AppendFormat(" {0,3:F0}", NumChords);
			}
			else
			{
				stringBuilder.AppendFormat("|{0,1:F0}", NumChords);
			}
			for (int i = 0; i < 8; i++)
			{
				if (!IsPDS)
				{
					if (i < shapeModelSource.Count)
					{
						if ("456".Contains(ShapeModelFit[i]))
						{
							stringBuilder.AppendFormat("|{0,4:F0}", ShapeModelDiameter[i]);
						}
						else if (ShapeModelFit[i] == "3")
						{
							stringBuilder.AppendFormat("|>{0,3:F0}", ShapeModelDiameter[i]);
						}
						else
						{
							stringBuilder.Append("|".PadRight(5));
						}
						stringBuilder.Append(" (" + ShapeModelSource[i] + ShapeModelNum[i].PadRight(7) + "," + ShapeModelFit[i] + ")");
					}
					else
					{
						stringBuilder.Append("|".PadRight(18));
					}
				}
				else
				{
					if (AsteroidNumber.Contains("P"))
					{
						continue;
					}
					if (i < shapeModelSource.Count)
					{
						if ("3456".Contains(shapeModelFit[i]))
						{
							if (shapeModelFit[i] == "3")
							{
								if (ShapeModelDiameter[i] > 0)
								{
									stringBuilder.Append("|" + (">" + string.Format("{0,1:F0}", ShapeModelDiameter[i])).PadLeft(4));
								}
								else
								{
									stringBuilder.Append("|-   ");
								}
							}
							else
							{
								stringBuilder.AppendFormat("|{0,4:F0}", ShapeModelDiameter[i]);
							}
							stringBuilder.Append("|" + (ShapeModelSource[i] + "|" + ShapeModelNum[i]).PadRight(4) + "|" + ShapeModelFit[i]);
						}
						else
						{
							stringBuilder.Append("|-   |" + (ShapeModelSource[i] + "|" + ShapeModelNum[i]).PadRight(4) + "|" + ShapeModelFit[i]);
						}
					}
					else
					{
						stringBuilder.Append("|-   |-|-   |-");
					}
				}
			}
			if (IsPDS)
			{
				stringBuilder.AppendFormat("|{0,4:F0}", year);
				stringBuilder.Append("-" + Month.ToString().PadLeft(2, '0') + "-" + Day.ToString().PadLeft(2, '0'));
			}
			return stringBuilder.ToString();
		}
	}
}
