using System;

namespace Occult
{
	public class DESourceFiles : IComparable
	{
		private string file;

		private string filename;

		private double startJD;

		private double endJD;

		private bool toBeUsed = true;

		internal string File
		{
			get
			{
				return file;
			}
			set
			{
				file = value;
			}
		}

		internal string FileName
		{
			get
			{
				return filename;
			}
			set
			{
				filename = value;
			}
		}

		internal double StartJD
		{
			get
			{
				return startJD;
			}
			set
			{
				startJD = value;
			}
		}

		internal double EndJD
		{
			get
			{
				return endJD;
			}
			set
			{
				endJD = value;
			}
		}

		internal bool ToBeUsed
		{
			get
			{
				return toBeUsed;
			}
			set
			{
				toBeUsed = value;
			}
		}

		public int CompareTo(object other)
		{
			return StartJD.CompareTo(((DESourceFiles)other).StartJD);
		}

		public override string ToString()
		{
			string text = Utilities.Date_from_JD(StartJD, 1, Use_BC: false);
			string text2 = Utilities.Date_from_JD(EndJD, 1, Use_BC: false);
			return FileName.PadRight(15) + text + " to " + text2;
		}
	}
}
