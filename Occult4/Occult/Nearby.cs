using System;

namespace Occult
{
	internal class Nearby : IComparable
	{
		private double magV = 30.0;

		private double magR = 30.0;

		private double sep;

		private string starID = "";

		public double MagV
		{
			get
			{
				return magV;
			}
			set
			{
				magV = value;
			}
		}

		public double MagR
		{
			get
			{
				return magR;
			}
			set
			{
				magR = value;
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

		public int CompareTo(object other)
		{
			return Sep.CompareTo(((Nearby)other).Sep);
		}

		public override string ToString()
		{
			return string.Format("{0,5:f1}\"  {1,5:f2}  {2,5:f2}  ", Sep, MagV, MagR) + StarID;
		}
	}
}
