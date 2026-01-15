using System;

namespace Occult.Asteroid_Observations
{
	internal class UnistellarObservers : IComparable
	{
		private bool positive;

		internal static int SortField;

		private string familyName = "";

		private string name = "";

		private string familyNameFirst;

		private int eventCount;

		private int positiveCount;

		public bool Positive
		{
			get
			{
				return positive;
			}
			set
			{
				positive = value;
			}
		}

		public int StationCount
		{
			get
			{
				return eventCount;
			}
			set
			{
				eventCount = value;
			}
		}

		public int PositiveCount
		{
			get
			{
				return positiveCount;
			}
			set
			{
				positiveCount = value;
			}
		}

		public string Name
		{
			get
			{
				return name;
			}
			set
			{
				name = value;
				string text = name;
				int num = text.IndexOf('+');
				if (num > 0)
				{
					text = text.Substring(0, num).Trim();
				}
				int num2 = text.IndexOf(' ');
				if (num2 > 0)
				{
					familyName = text.Substring(num2 + 1).Trim();
					familyNameFirst = familyName + ", " + text.Substring(0, num2).Trim();
				}
				else
				{
					familyName = (familyNameFirst = text);
				}
			}
		}

		public string FamilyName => familyName;

		public string FamilyNameFirst => familyNameFirst;

		public int CompareTo(object other)
		{
			if (SortField == 0)
			{
				return FamilyName.CompareTo(((UnistellarObservers)other).FamilyName);
			}
			if (SortField == 1)
			{
				if (StationCount.CompareTo(((UnistellarObservers)other).StationCount) == 0)
				{
					return FamilyName.CompareTo(((UnistellarObservers)other).FamilyName);
				}
				return ((UnistellarObservers)other).StationCount.CompareTo(StationCount);
			}
			if (PositiveCount.CompareTo(((UnistellarObservers)other).PositiveCount) == 0)
			{
				return FamilyName.CompareTo(((UnistellarObservers)other).FamilyName);
			}
			return ((UnistellarObservers)other).PositiveCount.CompareTo(PositiveCount);
		}

		public override string ToString()
		{
			return string.Format("{0,6}  {1,3}   ", StationCount, PositiveCount) + FamilyNameFirst;
		}
	}
}
