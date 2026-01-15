using System;

namespace Occult
{
	internal class LunarGrazeTags : IComparable
	{
		private float x;

		private float y;

		private float circleRadius;

		private string tag;

		private string date;

		private string starID;

		private string observerID;

		private int eventType;

		private bool currentEvent;

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

		internal float CircleRadius
		{
			get
			{
				return circleRadius;
			}
			set
			{
				circleRadius = value;
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

		internal string Date
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

		internal string StarID
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

		internal string ObserverID
		{
			get
			{
				return observerID;
			}
			set
			{
				observerID = value;
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

		internal bool CurrentEvent
		{
			get
			{
				return currentEvent;
			}
			set
			{
				currentEvent = value;
			}
		}

		public int CompareTo(object other)
		{
			return x.CompareTo(((LunarGrazeTags)other).X);
		}
	}
}
