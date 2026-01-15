using System;

namespace Occult.Asteroid_Observations
{
	internal class Asteroid_OBS_Tags : IComparable
	{
		private float x;

		private float y;

		private float dx;

		private float dy;

		private float rate;

		private int eventType;

		private string tag;

		internal float X
		{
			get
			{
				return x;
			}
			set
			{
				x = value;
			}
		}

		internal float Y
		{
			get
			{
				return y;
			}
			set
			{
				y = value;
			}
		}

		internal float dX
		{
			get
			{
				return dx;
			}
			set
			{
				dx = value;
			}
		}

		internal float dY
		{
			get
			{
				return dy;
			}
			set
			{
				dy = value;
			}
		}

		internal float RateMilliArcSec
		{
			get
			{
				return rate;
			}
			set
			{
				rate = value;
			}
		}

		internal int EventType
		{
			get
			{
				return eventType;
			}
			set
			{
				eventType = value;
			}
		}

		internal string Tag
		{
			get
			{
				return tag;
			}
			set
			{
				tag = value;
			}
		}

		public int CompareTo(object other)
		{
			return x.CompareTo(((Asteroid_OBS_Tags)other).X);
		}
	}
}
