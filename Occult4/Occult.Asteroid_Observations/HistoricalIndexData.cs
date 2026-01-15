using System;

namespace Occult.Asteroid_Observations
{
	internal class HistoricalIndexData : IComparable
	{
		private string asteroidNumber;

		private string asteroidID;

		private string display;

		private int year;

		private int month;

		private int day;

		private int numOfRecords;

		private int numOfChords;

		private int startRecord;

		private int asteroidNum;

		private int Arg1;

		private bool hasShapeModel;

		private bool hasUnfittedModels;

		internal static int SortField;

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

		public int AsteroidNum
		{
			get
			{
				return asteroidNum;
			}
			set
			{
				asteroidNum = value;
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

		private int AsteroidNo
		{
			get
			{
				int.TryParse(AsteroidNumber, out Arg1);
				return Arg1;
			}
		}

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

		public bool HasUnfittedModel
		{
			get
			{
				return hasUnfittedModels;
			}
			set
			{
				hasUnfittedModels = value;
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

		public int NumOfRecords
		{
			get
			{
				return numOfRecords;
			}
			set
			{
				numOfRecords = value;
			}
		}

		public int NumOfChords
		{
			get
			{
				return numOfChords;
			}
			set
			{
				numOfChords = value;
			}
		}

		public int StartRecord
		{
			get
			{
				return startRecord;
			}
			set
			{
				startRecord = value;
			}
		}

		public double SortDate => year * 10000 + month * 100 + day;

		public int CompareTo(object other)
		{
			switch (SortField)
			{
			case 0:
				if (AsteroidID == ((HistoricalIndexData)other).AsteroidID)
				{
					if (SortDate == ((HistoricalIndexData)other).SortDate)
					{
						return StartRecord.CompareTo(((HistoricalIndexData)other).StartRecord);
					}
					return SortDate.CompareTo(((HistoricalIndexData)other).SortDate);
				}
				return AsteroidID.CompareTo(((HistoricalIndexData)other).AsteroidID);
			case 1:
				if (AsteroidNum == ((HistoricalIndexData)other).AsteroidNum)
				{
					if (AsteroidNum == 0)
					{
						return AsteroidNumber.CompareTo(((HistoricalIndexData)other).AsteroidNumber);
					}
					if (SortDate == ((HistoricalIndexData)other).SortDate)
					{
						return StartRecord.CompareTo(((HistoricalIndexData)other).StartRecord);
					}
					return SortDate.CompareTo(((HistoricalIndexData)other).SortDate);
				}
				return AsteroidNum.CompareTo(((HistoricalIndexData)other).AsteroidNum);
			case 2:
				if (SortDate == ((HistoricalIndexData)other).SortDate)
				{
					return StartRecord.CompareTo(((HistoricalIndexData)other).StartRecord);
				}
				return SortDate.CompareTo(((HistoricalIndexData)other).SortDate);
			case 3:
				if (NumOfRecords == ((HistoricalIndexData)other).NumOfRecords)
				{
					if (NumOfChords == ((HistoricalIndexData)other).NumOfChords)
					{
						return SortDate.CompareTo(((HistoricalIndexData)other).SortDate);
					}
					return ((HistoricalIndexData)other).NumOfChords.CompareTo(NumOfChords);
				}
				return ((HistoricalIndexData)other).NumOfRecords.CompareTo(NumOfRecords);
			case 4:
				if (AsteroidNumber == ((HistoricalIndexData)other).AsteroidNumber)
				{
					return SortDate.CompareTo(((HistoricalIndexData)other).SortDate);
				}
				return AsteroidNumber.CompareTo(((HistoricalIndexData)other).AsteroidNumber);
			case 5:
				if (NumOfChords == ((HistoricalIndexData)other).NumOfChords)
				{
					if (NumOfRecords == ((HistoricalIndexData)other).NumOfRecords)
					{
						return SortDate.CompareTo(((HistoricalIndexData)other).SortDate);
					}
					return ((HistoricalIndexData)other).NumOfRecords.CompareTo(NumOfRecords);
				}
				return ((HistoricalIndexData)other).NumOfChords.CompareTo(NumOfChords);
			default:
				return 0;
			}
		}

		public void ReadIndexLine(string InLine)
		{
			display = InLine.Substring(0, 40);
			if (!int.TryParse(InLine.Substring(0, 4), out year))
			{
				year = 1900;
			}
			if (!int.TryParse(InLine.Substring(5, 2), out month))
			{
				month = 1;
			}
			if (!int.TryParse(InLine.Substring(8, 2), out day))
			{
				day = 1;
			}
			hasShapeModel = (InLine.Substring(10, 1) == "#") | (InLine.Substring(10, 1) == "†");
			hasUnfittedModels = InLine.Substring(10, 1) == "†";
			asteroidNumber = InLine.Substring(11, 7).Trim();
			if (!int.TryParse(asteroidNumber, out asteroidNum))
			{
				asteroidNum = 0;
			}
			asteroidID = InLine.Substring(19, 13);
			if (!int.TryParse(InLine.Substring(33, 3), out numOfRecords))
			{
				numOfRecords = 0;
			}
			if (!int.TryParse(InLine.Substring(37, 3), out numOfChords))
			{
				numOfChords = 0;
			}
			if (!int.TryParse(InLine.Substring(40), out startRecord))
			{
				startRecord = 0;
			}
		}

		public override string ToString()
		{
			return display;
		}
	}
}
