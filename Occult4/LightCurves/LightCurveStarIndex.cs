using System;
using System.Text;

namespace LightCurves
{
	internal class LightCurveStarIndex : IComparable
	{
		private int starNum1;

		private int starNum2;

		private string date = "";

		private string name = "";

		private double jD;

		private int lightCurveIndex;

		private int catalogueNumber;

		private string observer = "";

		private string[] CatName = new string[8] { "Hip", "SAO", "XZ80Q", "ZC", "Tyc2", "UCAC4", "", "" };

		internal static bool SortByDate;

		internal static bool SortByName;

		public string StarNumber
		{
			get
			{
				if (StarNum2 > 0)
				{
					return StarNum1_or_AsteroidNo + "-" + StarNum2;
				}
				return StarNum1_or_AsteroidNo.ToString();
			}
			set
			{
				string[] array = value.Split(new char[1] { '-' });
				starNum1 = int.Parse(array[0]);
				if (array.Length > 1)
				{
					starNum2 = int.Parse(array[1]);
				}
			}
		}

		public int StarNum1_or_AsteroidNo
		{
			get
			{
				return starNum1;
			}
			set
			{
				starNum1 = value;
			}
		}

		public int StarNum2
		{
			get
			{
				return starNum2;
			}
			set
			{
				starNum2 = value;
			}
		}

		public string ObjectName
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

		public double JD
		{
			get
			{
				return jD;
			}
			set
			{
				jD = value;
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

		public int LightCurveIndex
		{
			get
			{
				return lightCurveIndex;
			}
			set
			{
				lightCurveIndex = value;
			}
		}

		public int CatalogueNumber
		{
			get
			{
				return catalogueNumber;
			}
			set
			{
				catalogueNumber = value;
			}
		}

		public string Observer
		{
			get
			{
				return observer;
			}
			set
			{
				observer = value;
			}
		}

		public void SetValues(int Index, int Num1, int num2, string EventDate, double jday)
		{
			lightCurveIndex = Index;
			starNum1 = Num1;
			starNum2 = num2;
			date = EventDate;
			jD = jday;
		}

		public void SetObserverNames(int Index, string ObserverName, string EventDate, double jday)
		{
			lightCurveIndex = Index;
			Observer = ObserverName;
			date = EventDate;
			jD = jday;
		}

		public void SetAsteroidNames(int Index, string AsteroidName, string EventDate, double jday)
		{
			lightCurveIndex = Index;
			ObjectName = AsteroidName;
			date = EventDate;
			jD = jday;
		}

		public void SetAsteroidNumbers(int Index, int AsteroidNumber, string EventDate, double jday)
		{
			lightCurveIndex = Index;
			StarNum1_or_AsteroidNo = AsteroidNumber;
			date = EventDate;
			jD = jday;
		}

		public int CompareTo(object other)
		{
			if (SortByDate)
			{
				return JD.CompareTo(((LightCurveStarIndex)other).JD);
			}
			if (SortByName)
			{
				if (ObjectName == ((LightCurveStarIndex)other).ObjectName)
				{
					if (StarNum2 == ((LightCurveStarIndex)other).StarNum2)
					{
						return JD.CompareTo(((LightCurveStarIndex)other).JD);
					}
					return StarNum2.CompareTo(((LightCurveStarIndex)other).StarNum2);
				}
				return ObjectName.CompareTo(((LightCurveStarIndex)other).ObjectName);
			}
			if (StarNum1_or_AsteroidNo == ((LightCurveStarIndex)other).StarNum1_or_AsteroidNo)
			{
				if (StarNum2 == ((LightCurveStarIndex)other).StarNum2)
				{
					return JD.CompareTo(((LightCurveStarIndex)other).JD);
				}
				return StarNum2.CompareTo(((LightCurveStarIndex)other).StarNum2);
			}
			return StarNum1_or_AsteroidNo.CompareTo(((LightCurveStarIndex)other).StarNum1_or_AsteroidNo);
		}

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			switch (CatalogueNumber)
			{
			case 0:
				stringBuilder.AppendFormat(CatName[0] + " {0,6} ", StarNum1_or_AsteroidNo);
				break;
			case 1:
				stringBuilder.AppendFormat(CatName[1] + " {0,6} ", StarNum1_or_AsteroidNo);
				break;
			case 2:
				stringBuilder.AppendFormat(CatName[2] + string.Format(" {0,1}", StarNum1_or_AsteroidNo).PadRight(8));
				break;
			case 3:
				stringBuilder.AppendFormat(CatName[3] + string.Format(" {0,1}", StarNum1_or_AsteroidNo).PadRight(6));
				break;
			case 4:
				stringBuilder.AppendFormat(CatName[4] + string.Format(" {0,4}-{1,1}", StarNum1_or_AsteroidNo, StarNum2).PadRight(12));
				break;
			case 5:
				stringBuilder.AppendFormat(CatName[5] + string.Format(" {0,3}-{1,6}", StarNum1_or_AsteroidNo, StarNum2).PadRight(12));
				break;
			case 6:
				stringBuilder.Append(string.Format("({0,1}) ", StarNum1_or_AsteroidNo).PadRight(9));
				break;
			case 7:
				stringBuilder.Append(Observer.PadRight(16).Substring(0, 16));
				break;
			case 8:
				stringBuilder.Append(ObjectName.PadRight(14).Substring(0, 14));
				break;
			}
			stringBuilder.Append(" " + Date);
			return stringBuilder.ToString();
		}
	}
}
