using System;

namespace LightCurves
{
	internal class Sites : IComparable
	{
		private string id;

		private string lonDeg;

		private string longMin;

		private string longSec;

		private string latDeg;

		private string latMin;

		private string latSec;

		private string alt;

		private string observer;

		internal string ID
		{
			get
			{
				return id;
			}
			set
			{
				id = value;
			}
		}

		internal string LonDeg
		{
			get
			{
				return lonDeg;
			}
			set
			{
				lonDeg = value;
			}
		}

		internal string LongMin
		{
			get
			{
				return longMin;
			}
			set
			{
				longMin = value;
			}
		}

		internal string LongSec
		{
			get
			{
				return longSec;
			}
			set
			{
				longSec = value;
			}
		}

		internal string LatDeg
		{
			get
			{
				return latDeg;
			}
			set
			{
				latDeg = value;
			}
		}

		internal string LatMin
		{
			get
			{
				return latMin;
			}
			set
			{
				latMin = value;
			}
		}

		internal string LatSec
		{
			get
			{
				return latSec;
			}
			set
			{
				latSec = value;
			}
		}

		internal string Alt
		{
			get
			{
				return alt;
			}
			set
			{
				alt = value;
			}
		}

		internal string Observer
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

		internal string Entry => ID + "\r\n" + string.Format("{0,4} {1,3} {2,5:f1}\r\n", LonDeg, LongMin, LongSec) + string.Format("{0,3} {1,3} {2,5:f1}\r\n", LatDeg, LatMin, LatSec) + Alt.ToString() + "m  " + Observer;

		public int CompareTo(object other)
		{
			return Observer.CompareTo(((Sites)other).Observer);
		}
	}
}
