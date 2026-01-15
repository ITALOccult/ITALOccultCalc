using System;
using System.Collections.Generic;

namespace Occult.Asteroid_Observations
{
	internal class LinesForAnEvent : IComparable
	{
		private string line;

		private string indexLine = "";

		private string astName = "";

		private string starID = "";

		private double eventJD;

		private double eventReferenceJD;

		private int chordCount;

		private int astNum;

		private bool isXML;

		private bool obsFileisCurrentVersion = true;

		private string currentFileVersion = "";

		public List<string> Lines = new List<string>();

		public double EventJD
		{
			get
			{
				return eventJD;
			}
			set
			{
				eventJD = value;
			}
		}

		public double EventReferenceJD
		{
			get
			{
				return eventReferenceJD;
			}
			set
			{
				eventReferenceJD = value;
			}
		}

		public string IndexLine
		{
			get
			{
				return indexLine;
			}
			set
			{
				indexLine = value;
			}
		}

		public bool IsXML
		{
			get
			{
				return isXML;
			}
			set
			{
				isXML = value;
			}
		}

		internal bool ObsFileisCurrentVersion
		{
			get
			{
				return obsFileisCurrentVersion;
			}
			set
			{
				obsFileisCurrentVersion = value;
			}
		}

		internal string CurrentFileVersion
		{
			get
			{
				return currentFileVersion;
			}
			set
			{
				currentFileVersion = value;
			}
		}

		public int AstNum
		{
			get
			{
				return astNum;
			}
			set
			{
				astNum = value;
			}
		}

		public string AstName
		{
			get
			{
				return astName;
			}
			set
			{
				astName = value;
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

		public string Line
		{
			get
			{
				return line;
			}
			set
			{
				line = value;
			}
		}

		public int ChordCount
		{
			get
			{
				return chordCount;
			}
			set
			{
				chordCount = value;
			}
		}

		public int CompareTo(object other)
		{
			if (EventJD.CompareTo(((LinesForAnEvent)other).EventJD) == 0)
			{
				if (EventReferenceJD.CompareTo(((LinesForAnEvent)other).EventReferenceJD) == 0)
				{
					if (AstNum.CompareTo(((LinesForAnEvent)other).AstNum) == 0)
					{
						return AstName.CompareTo(((LinesForAnEvent)other).AstName);
					}
					return AstNum.CompareTo(((LinesForAnEvent)other).AstNum);
				}
				return EventReferenceJD.CompareTo(((LinesForAnEvent)other).EventReferenceJD);
			}
			return EventJD.CompareTo(((LinesForAnEvent)other).EventJD);
		}
	}
}
