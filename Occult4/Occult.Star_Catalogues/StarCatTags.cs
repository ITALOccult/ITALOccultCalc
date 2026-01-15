using System;

namespace Occult.Star_Catalogues
{
	internal class StarCatTags : IComparable
	{
		private float x;

		private float y;

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
			return x.CompareTo(((StarCatTags)other).X);
		}
	}
}
